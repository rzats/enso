package org.enso.languageserver.text

import akka.actor.{Actor, ActorRef, Props, Terminated}
import org.enso.languageserver.LanguageProtocol.{
  AcquireCapability,
  ReleaseCapability
}
import org.enso.languageserver.text.TextProtocol.OpenFile
import org.enso.languageserver.data.{
  CanEdit,
  CapabilityRegistration,
  ContentDigest
}
import org.enso.languageserver.filemanager.Path

class BufferRegistry(fileManager: ActorRef)(
  implicit contentDigest: ContentDigest
) extends Actor {

  override def receive: Receive = running(Map.empty)

  private def running(registry: Map[Path, ActorRef]): Receive = {
    case msg @ OpenFile(_, path) =>
      if (registry.contains(path)) {
        registry(path).forward(msg)
      } else {
        val bufferRef =
          context.actorOf(CollaborativeBuffer.props(path, fileManager))
        context.watch(bufferRef)
        bufferRef.forward(msg)
        context.become(running(registry + (path -> bufferRef)))
      }

    case Terminated(bufferRef) =>
      context.become(running(registry.filter(_._2 != bufferRef)))

    case msg @ AcquireCapability(_, CapabilityRegistration(CanEdit(path))) =>
      registry(path).forward(msg) //todo error handling

    case msg @ ReleaseCapability(_, CapabilityRegistration(CanEdit(path))) =>
      registry(path).forward(msg) //todo error handling
  }

}

object BufferRegistry {

  def props(
    fileManager: ActorRef
  )(implicit contentDigest: ContentDigest): Props =
    Props(new BufferRegistry(fileManager))

}
