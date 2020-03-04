package org.enso.languageserver.text

import akka.actor.{Actor, ActorRef, Props, Stash}
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
    with Stash {

  import context.dispatcher

  override def receive: Receive = waiting

  private def waiting: Receive = {
    case OpenFile(client, path) if bufferPath == path =>
      context.system.eventStream.publish(BufferCreated(path))
      fileManager ! FileManagerProtocol.ReadFile(path)
      context.system.scheduler
        .scheduleOnce(10 seconds, self, FileReadingTimeout)
      context.become(reading(client, sender()))
  }

  private def reading(client: Client.Id, originalSender: ActorRef): Receive = {
    case ReadFileResult(Right(content)) =>
      val buffer = Buffer(content)
      val cap    = CapabilityRegistration(CanEdit(bufferPath))
      originalSender ! OpenFileResponse(
        Right(OpenFileResult(buffer, Some(cap)))
      )
      unstashAll()
      context.become(editing(buffer, Set(client), client))

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
    clients: Set[Client.Id],
    writeLock: Client.Id
  ): Receive = {
    case OpenFile(client, _) =>
      context.become(editing(buffer, clients + client, writeLock))
      sender ! OpenFileResponse(Right(OpenFileResult(buffer, None)))
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
