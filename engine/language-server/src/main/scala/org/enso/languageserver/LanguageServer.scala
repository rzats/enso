package org.enso.languageserver

import akka.actor.{Actor, ActorLogging, ActorRef, Stash}
import cats.effect.IO
import org.enso.languageserver.data._
import org.enso.languageserver.filemanager.FileManagerProtocol._
import org.enso.languageserver.filemanager.{FileSystemApi, FileSystemObject}

object LanguageProtocol {

  /** Initializes the Language Server. */
  case object Initialize

  /**
    * Notifies the Language Server about a new client connecting.
    *
    * @param clientId the internal client id.
    * @param clientActor the actor this client is represented by.
    */
  case class Connect(clientId: Client.Id, clientActor: ActorRef)

  /**
    * Notifies the Language Server about a client disconnecting.
    * The client may not send any further messages after this one.
    *
    * @param clientId the id of the disconnecting client.
    */
  case class Disconnect(clientId: Client.Id)

  /**
    * Requests the Language Server grant a new capability to a client.
    *
    * @param clientId the client to grant the capability to.
    * @param registration the capability to grant.
    */
  case class AcquireCapability(
    client: Client,
    registration: CapabilityRegistration
  )

  sealed trait AcquireCapabilityResponse
  case object CapabilityAcquired              extends AcquireCapabilityResponse
  case object CapabilityAcquisitionBadRequest extends AcquireCapabilityResponse

  /**
    * Notifies the Language Server about a client releasing a capability.
    *
    * @param clientId the client releasing the capability.
    * @param capabilityId the capability being released.
    */
  case class ReleaseCapability(
    clientId: Client.Id,
    capability: CapabilityRegistration
  )

  sealed trait ReleaseCapabilityResponse
  case object CapabilityReleased          extends ReleaseCapabilityResponse
  case object CapabilityReleaseBadRequest extends ReleaseCapabilityResponse

  /**
    * A notification sent by the Language Server, notifying a client about
    * a capability being taken away from them.
    *
    * @param capabilityId the capability being released.
    */
  case class CapabilityForceReleased(capability: CapabilityRegistration)

  /**
    * A notification sent by the Language Server, notifying a client about a new
    * capability being granted to them.
    *
    * @param registration the capability being granted.
    */
  case class CapabilityGranted(registration: CapabilityRegistration)
}

/**
  * An actor representing an instance of the Language Server.
  *
  * @param config the configuration used by this Language Server.
  */
class LanguageServer(config: Config, fs: FileSystemApi[IO])
    extends Actor
    with Stash
    with ActorLogging {
  import LanguageProtocol._

  override def receive: Receive = {
    case Initialize =>
      log.debug("Language Server initialized.")
      unstashAll()
      context.become(initialized(config))
    case _ => stash()
  }

  def initialized(
    config: Config,
    env: Environment = Environment.empty
  ): Receive = {
    case Connect(clientId, actor) =>
      log.debug("Client connected [{}].", clientId)
      context.become(
        initialized(config, env.addClient(Client(clientId, actor)))
      )

    case Disconnect(clientId) =>
      log.debug("Client disconnected [{}].", clientId)
      context.become(initialized(config, env.removeClient(clientId)))

    case WriteFile(path, content) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          _        <- fs.write(path.toFile(rootPath), content).unsafeRunSync()
        } yield ()

      sender ! WriteFileResult(result)

    case ReadFile(path) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          content  <- fs.read(path.toFile(rootPath)).unsafeRunSync()
        } yield content

      sender ! ReadFileResult(result)

    case CreateFile(FileSystemObject.File(name, path)) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          _        <- fs.createFile(path.toFile(rootPath, name)).unsafeRunSync()
        } yield ()

      sender ! CreateFileResult(result)

    case CreateFile(FileSystemObject.Directory(name, path)) =>
      val result =
        for {
          rootPath <- config.findContentRoot(path.rootId)
          _        <- fs.createDirectory(path.toFile(rootPath, name)).unsafeRunSync()
        } yield ()

      sender ! CreateFileResult(result)

  }
  /* Note [Usage of unsafe methods]
     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
     It invokes side-effecting function, all exceptions are caught and
     explicitly returned as left side of disjunction.
 */
}
