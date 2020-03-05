package org.enso.languageserver

import java.util.UUID

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import akka.pattern.ask
import akka.util.Timeout
import org.enso.languageserver.ClientApi._
import org.enso.languageserver.data.{CanEdit, CapabilityRegistration, Client}
import org.enso.languageserver.event.client.ClientDisconnected
import org.enso.languageserver.filemanager.FileManagerApi._
import org.enso.languageserver.filemanager.FileManagerProtocol.{
  CreateFileResult,
  WriteFileResult
}
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  FileSystemFailureMapper
}
import org.enso.languageserver.jsonrpc.Errors.ServiceError
import org.enso.languageserver.jsonrpc._
import org.enso.languageserver.requesthandler.{
  AcquireCapabilityHandler,
  ReleaseCapabilityHandler
}
import org.enso.languageserver.text.{TextApi, TextProtocol}
import org.enso.languageserver.text.TextApi.OpenFile

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

  case object ReleaseCapability extends Method("capability/release") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = CapabilityRegistration
    }
    implicit val hasResult = new HasResult[this.type] {
      type Result = Unused.type
    }
  }

  case object ForceReleaseCapability
      extends Method("capability/forceReleased") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = CapabilityRegistration
    }
  }

  case object GrantCapability extends Method("capability/granted") {
    implicit val hasParams = new HasParams[this.type] {
      type Params = CapabilityRegistration
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
  val bufferRegistry: ActorRef,
  val capabilityRouter: ActorRef,
  requestTimeout: FiniteDuration = 10.seconds
) extends Actor
    with Stash
    with ActorLogging {

  import context.dispatcher

  implicit val timeout = Timeout(requestTimeout)

  private val client = Client(clientId, self)

  private val requestHandlers: Map[Any, Props] =
    Map(
      AcquireCapability -> AcquireCapabilityHandler
        .props(capabilityRouter, requestTimeout, client),
      ReleaseCapability -> ReleaseCapabilityHandler
        .props(capabilityRouter, requestTimeout, client)
    )

  override def receive: Receive = {
    case ClientApi.WebConnect(webActor) =>
      unstashAll()
      context.become(connected(webActor))
    case _ => stash()
  }

  def connected(webActor: ActorRef): Receive = {
    case MessageHandler.Disconnected =>
      server ! LanguageProtocol.Disconnect(clientId)
      context.system.eventStream.publish(ClientDisconnected(clientId))
      context.stop(self)

    case LanguageProtocol.CapabilityForceReleased(registration) =>
      webActor ! Notification(ForceReleaseCapability, registration)

    case LanguageProtocol.CapabilityGranted(registration) =>
      webActor ! Notification(GrantCapability, registration)

    case r @ Request(method, _, _) if (requestHandlers.contains(method)) =>
      val handler = context.actorOf(requestHandlers(method))
      handler.forward(r)

    case Request(OpenFile, id, params: OpenFile.Params) =>
      openFile(webActor, id, params)

    case Request(WriteFile, id, params: WriteFile.Params) =>
      writeFile(webActor, id, params)

    case Request(ReadFile, id, params: ReadFile.Params) =>
      readFile(webActor, id, params)

    case Request(CreateFile, id, params: CreateFile.Params) =>
      createFile(webActor, id, params)
  }

  private def openFile(
    webActor: ActorRef,
    id: Id,
    params: OpenFile.Params
  ): Unit = {
    (bufferRegistry ? TextProtocol.OpenFile(
      Client(clientId, self),
      params.path
    )).onComplete {
      case Success(
          TextProtocol.OpenFileResponse(
            Right(TextProtocol.OpenFileResult(buffer, capability))
          )
          ) =>
        webActor ! ResponseResult(
          OpenFile,
          id,
          OpenFile
            .Result(capability, buffer.contents.toString, buffer.version)
        )

      case Success(TextProtocol.OpenFileResponse(Left(failure))) =>
        webActor ! ResponseError(
          Some(id),
          FileSystemFailureMapper.mapFailure(failure)
        )

      case Failure(th) =>
        log.error("An exception occurred during opening file", th)
        webActor ! ResponseError(Some(id), ServiceError)
    }
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

}
