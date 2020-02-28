package org.enso.languageserver

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import akka.pattern.ask
import akka.util.Timeout
import org.enso.languageserver.ClientApi._
import org.enso.languageserver.LanguageProtocol.OpenFileResponse
import org.enso.languageserver.data.{CapabilityRegistration, Client}
import org.enso.languageserver.filemanager.FileManagerApi._
import org.enso.languageserver.filemanager.FileManagerProtocol.{
  CreateFileResult,
  WriteFileResult
}
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper,
  Path
}
import org.enso.languageserver.jsonrpc.Errors.ServiceError
import org.enso.languageserver.jsonrpc._

import scala.concurrent.duration._
import scala.util.{Failure, Success}

/**
  * The JSON RPC API provided by the language server.
  * See [[https://github.com/luna/enso/blob/master/doc/design/engine/engine-services.md]]
  * for message specifications.
  */
object ClientApi {
  import io.circe.generic.auto._

  case object AcquireCapability extends Method("capability/acquire") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = CapabilityRegistration
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case class ReleaseCapabilityParams(id: UUID)

  case object ReleaseCapability extends Method("capability/release") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = ReleaseCapabilityParams
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ForceReleaseCapability
      extends Method("capability/forceReleased") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = ReleaseCapabilityParams
    }
  }

  case object GrantCapability extends Method("capability/granted") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = CapabilityRegistration
    }
  }

  case object OpenFile extends Method("text/openFile") {
    case class Params(path: Path)
    case class Result(
      writeCapability: Option[CapabilityRegistration],
      content: String,
      currentVersion: UUID
    )
    implicit val hasParams = new HasParams[this.type] {
      type Params = OpenFile.Params
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = OpenFile.Result
    }
  }

  val protocol: Protocol = Protocol.empty
    .registerRequest(AcquireCapability)
    .registerRequest(ReleaseCapability)
    .registerRequest(WriteFile)
    .registerRequest(ReadFile)
    .registerRequest(CreateFile)
    .registerRequest(OpenFile)
    .registerNotification(ForceReleaseCapability)
    .registerNotification(GrantCapability)

  case class WebConnect(webActor: ActorRef)
}

/**
  * An actor handling communications between a single client and the language
  * server.
  *
  * @param clientId the internal client id.
  * @param server the language server actor.
  */
class ClientController(
  val clientId: Client.Id,
  val server: ActorRef,
  requestTimeout: FiniteDuration = 10.seconds
) extends Actor
    with Stash
    with ActorLogging {

  import context.dispatcher

  implicit val timeout = Timeout(requestTimeout)

  override def receive: Receive = {
    case ClientApi.WebConnect(webActor) =>
      unstashAll()
      context.become(connected(webActor))
    case _ => stash()
  }

  def connected(webActor: ActorRef): Receive = {
    case MessageHandler.Disconnected =>
      server ! LanguageProtocol.Disconnect(clientId)
      context.stop(self)

    case LanguageProtocol.CapabilityForceReleased(id) =>
      webActor ! Notification(
        ForceReleaseCapability,
        ReleaseCapabilityParams(id)
      )

    case LanguageProtocol.CapabilityGranted(registration) =>
      webActor ! Notification(GrantCapability, registration)

    case Request(AcquireCapability, id, registration: CapabilityRegistration) =>
      server ! LanguageProtocol.AcquireCapability(clientId, registration)
      sender ! ResponseResult(AcquireCapability, id, Unused)

    case Request(ReleaseCapability, id, params: ReleaseCapabilityParams) =>
      server ! LanguageProtocol.ReleaseCapability(clientId, params.id)
      sender ! ResponseResult(ReleaseCapability, id, Unused)

    case Request(WriteFile, id, params: WriteFile.Params) =>
      writeFile(webActor, id, params)

    case Request(ReadFile, id, params: ReadFile.Params) =>
      readFile(webActor, id, params)

    case Request(CreateFile, id, params: CreateFile.Params) =>
      createFile(webActor, id, params)

    case Request(OpenFile, id, params: OpenFile.Params) =>
      openFile(webActor, id, params)
  }

  private def readFile(
    webActor: ActorRef,
    id: Id,
    params: ReadFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.ReadFile(params.path)).onComplete {
      case Success(
          FileManagerProtocol.ReadFileResult(Right(content: String))
          ) =>
        webActor ! ResponseResult(ReadFile, id, ReadFile.Result(content))

      case Success(FileManagerProtocol.ReadFileResult(Left(failure))) =>
        webActor ! ResponseError(
          Some(id),
          FileSystemFailureMapper.mapFailure(failure)
        )

      case Failure(th) =>
        log.error("An exception occurred during reading a file", th)
        webActor ! ResponseError(Some(id), ServiceError)
    }
  }

  private def writeFile(
    webActor: ActorRef,
    id: Id,
    params: WriteFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.WriteFile(params.path, params.contents))
      .onComplete {
        case Success(WriteFileResult(Right(()))) =>
          webActor ! ResponseResult(WriteFile, id, Unused)

        case Success(WriteFileResult(Left(failure))) =>
          webActor ! ResponseError(
            Some(id),
            FileSystemFailureMapper.mapFailure(failure)
          )

        case Failure(th) =>
          log.error("An exception occurred during writing to a file", th)
          webActor ! ResponseError(Some(id), ServiceError)
      }
  }

  private def createFile(
    webActor: ActorRef,
    id: Id,
    params: CreateFile.Params
  ): Unit = {
    (server ? FileManagerProtocol.CreateFile(params.`object`))
      .onComplete {
        case Success(CreateFileResult(Right(()))) =>
          webActor ! ResponseResult(CreateFile, id, Unused)

        case Success(CreateFileResult(Left(failure))) =>
          webActor ! ResponseError(
            Some(id),
            FileSystemFailureMapper.mapFailure(failure)
          )

        case Failure(th) =>
          log.error("An exception occurred during creating a file", th)
          webActor ! ResponseError(Some(id), ServiceError)
      }
  }

  private def openFile(
    webActor: ActorRef,
    id: Id,
    params: OpenFile.Params
  ): Unit = {
    (server ? LanguageProtocol.OpenFile(clientId, params.path))
      .onComplete {
        case Success(
            LanguageProtocol.OpenFileResponse(
              Right(LanguageProtocol.OpenFileResult(buffer, capability))
            )
            ) =>
          webActor ! ResponseResult(
            OpenFile,
            id,
            OpenFile.Result(capability, buffer.contents, buffer.version)
          )

        case Success(OpenFileResponse(Left(failure))) =>
          webActor ! ResponseError(
            Some(id),
            FileSystemFailureMapper.mapFailure(failure)
          )

        case Failure(th) =>
          log.error("An exception occurred during opening file", th)
          webActor ! ResponseError(Some(id), ServiceError)
      }
  }

}
