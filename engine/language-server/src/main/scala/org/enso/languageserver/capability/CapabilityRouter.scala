package org.enso.languageserver.capability

import akka.actor.{Actor, ActorRef, Props}
import org.enso.languageserver.LanguageProtocol.{
  AcquireCapability,
  ReleaseCapability
}
import org.enso.languageserver.data.{CanEdit, CapabilityRegistration}

class CapabilityRouter(bufferRegistry: ActorRef) extends Actor {

  override def receive: Receive = {
    case msg @ AcquireCapability(_, CapabilityRegistration(CanEdit(_))) =>
      bufferRegistry.forward(msg)

    case msg @ ReleaseCapability(_, CapabilityRegistration(CanEdit(_))) =>
      bufferRegistry.forward(msg)

  }

}

object CapabilityRouter {

  def props(bufferRegistry: ActorRef): Props =
    Props(new CapabilityRouter(bufferRegistry))

}
