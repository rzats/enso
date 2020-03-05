package org.enso.languageserver.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.ClientApi.AcquireCapability
import org.enso.languageserver.LanguageProtocol
import org.enso.languageserver.LanguageProtocol.{
  CapabilityAcquired,
  CapabilityAcquisitionBadRequest
}
import org.enso.languageserver.data.{CapabilityRegistration, Client}
import org.enso.languageserver.jsonrpc.Errors.ServiceError
import org.enso.languageserver.jsonrpc._

import scala.concurrent.duration.FiniteDuration

class AcquireCapabilityHandler(
  capabilityRouter: ActorRef,
  requestTimeout: FiniteDuration,
  client: Client
) extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(AcquireCapability, id, registration: CapabilityRegistration) =>
      capabilityRouter ! LanguageProtocol.AcquireCapability(
        client,
        registration
      )
      context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case RequestTimeout =>
      log.error(s"Acquiring capability for ${client.id} timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case CapabilityAcquired =>
      replyTo ! ResponseResult(AcquireCapability, id, Unused)
      context.stop(self)

    case CapabilityAcquisitionBadRequest =>
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)
  }

}

object AcquireCapabilityHandler {

  def props(
    capabilityRouter: ActorRef,
    requestTimeout: FiniteDuration,
    client: Client
  ): Props =
    Props(
      new AcquireCapabilityHandler(capabilityRouter, requestTimeout, client)
    )

}
