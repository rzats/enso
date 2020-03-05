package org.enso.languageserver.text

import akka.actor.{Actor, ActorLogging, ActorRef, Props, Stash}
import org.enso.languageserver.LanguageProtocol._
import org.enso.languageserver.data.Client.Id
import org.enso.languageserver.data.{
  CanEdit,
  CapabilityRegistration,
  Client,
  ContentDigest
}
import org.enso.languageserver.event.buffer.{BufferClosed, BufferCreated}
import org.enso.languageserver.filemanager.FileManagerProtocol.ReadFileResult
import org.enso.languageserver.filemanager.{
  FileManagerProtocol,
  OperationTimeout,
  Path
}
import org.enso.languageserver.text.CollaborativeBuffer.FileReadingTimeout
import org.enso.languageserver.text.TextProtocol.{
  OpenFile,
  OpenFileResponse,
  OpenFileResult
}

import scala.concurrent.duration._
import scala.language.postfixOps

class CollaborativeBuffer(bufferPath: Path, fileManager: ActorRef)(
  implicit contentDigest: ContentDigest
) extends Actor
    with Stash
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = waiting

  private def waiting: Receive = {
    case OpenFile(client, path) =>
      context.system.eventStream.publish(BufferCreated(path))
      fileManager ! FileManagerProtocol.ReadFile(path)
      context.system.scheduler
        .scheduleOnce(10 seconds, self, FileReadingTimeout)
      context.become(reading(client, sender()))
      log.info(s"Buffer $path opened for ${client.id}")
  }

  private def reading(client: Client, originalSender: ActorRef): Receive = {
    case ReadFileResult(Right(content)) =>
      val buffer = Buffer(content)
      val cap    = CapabilityRegistration(CanEdit(bufferPath))
      originalSender ! OpenFileResponse(
        Right(OpenFileResult(buffer, Some(cap)))
      )
      unstashAll()
      context.become(editing(buffer, Map(client.id -> client), Some(client)))

    case ReadFileResult(Left(failure)) =>
      originalSender ! OpenFileResponse(Left(failure))
      stop()

    case FileReadingTimeout =>
      originalSender ! OpenFileResponse(Left(OperationTimeout))
      stop()

    case _ => stash()
  }

  private def editing(
    buffer: Buffer,
    clients: Map[Client.Id, Client],
    maybeWriteLock: Option[Client]
  ): Receive = {
    case OpenFile(client, _) =>
      val writeCapability =
        if (maybeWriteLock.isEmpty)
          Some(CapabilityRegistration(CanEdit(bufferPath)))
        else
          None
      sender ! OpenFileResponse(Right(OpenFileResult(buffer, writeCapability)))
      context.become(
        editing(buffer, clients + (client.id -> client), maybeWriteLock)
      )

    case AcquireCapability(clientId, CapabilityRegistration(CanEdit(path))) =>
      acquireWriteLock(buffer, clients, maybeWriteLock, clientId, path)

    case ReleaseCapability(clientId, CapabilityRegistration(CanEdit(_))) =>
      releaseWriteLock(buffer, clients, maybeWriteLock, clientId)

  }

  private def releaseWriteLock(
    buffer: Buffer,
    clients: Map[Client.Id, Client],
    maybeWriteLock: Option[Client],
    clientId: Id
  ): Unit = {
    maybeWriteLock match {
      case None =>
        sender() ! CapabilityReleaseBadRequest
        context.become(editing(buffer, clients, maybeWriteLock))

      case Some(holder) if holder.id != clientId =>
        sender() ! CapabilityReleaseBadRequest
        context.become(editing(buffer, clients, maybeWriteLock))

      case Some(holder) if holder.id == clientId =>
        sender() ! CapabilityReleased
        context.become(editing(buffer, clients, None))
    }
  }

  private def acquireWriteLock(
    buffer: Buffer,
    clients: Map[Client.Id, Client],
    maybeWriteLock: Option[Client],
    clientId: Client,
    path: Path
  ): Unit = {
    maybeWriteLock match {
      case None =>
        sender() ! CapabilityAcquired
        context.become(editing(buffer, clients, Some(clientId)))

      case Some(holder) if holder == clientId =>
        sender() ! CapabilityAcquisitionBadRequest
        context.become(editing(buffer, clients, maybeWriteLock))

      case Some(holder) if holder != clientId =>
        sender() ! CapabilityAcquired
        holder.actor ! CapabilityForceReleased(
          CapabilityRegistration(CanEdit(path))
        )
        context.become(editing(buffer, clients, Some(clientId)))
    }
  }

  def stop(): Unit = {
    context.system.eventStream.publish(BufferClosed(bufferPath))
    context.stop(self)
  }

}

object CollaborativeBuffer {

  case object FileReadingTimeout

  def props(
    bufferPath: Path,
    fileManager: ActorRef
  )(implicit contentDigest: ContentDigest): Props =
    Props(new CollaborativeBuffer(bufferPath, fileManager))

}
