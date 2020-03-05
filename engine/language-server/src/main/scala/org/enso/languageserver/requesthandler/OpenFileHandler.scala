package org.enso.languageserver.requesthandler

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.enso.languageserver.LanguageProtocol
import org.enso.languageserver.data.Client
import org.enso.languageserver.filemanager.FileSystemFailureMapper
import org.enso.languageserver.jsonrpc.Errors.ServiceError
import org.enso.languageserver.jsonrpc.{
  Id,
  Request,
  ResponseError,
  ResponseResult
}
import org.enso.languageserver.text.TextApi.OpenFile
import org.enso.languageserver.text.TextProtocol
import org.enso.languageserver.text.TextProtocol.{
  OpenFileResponse,
  OpenFileResult
}

import scala.concurrent.duration.FiniteDuration

class OpenFileHandler(
  bufferRegistry: ActorRef,
  requestTimeout: FiniteDuration,
  client: Client
) extends Actor
    with ActorLogging {

  import context.dispatcher

  override def receive: Receive = requestStage

  private def requestStage: Receive = {
    case Request(OpenFile, id, params: OpenFile.Params) =>
      bufferRegistry ! TextProtocol.OpenFile(client, params.path)
      context.system.scheduler
        .scheduleOnce(requestTimeout, self, RequestTimeout)
      context.become(responseStage(id, sender()))
  }

  private def responseStage(id: Id, replyTo: ActorRef): Receive = {
    case RequestTimeout =>
      log.error(s"Opening file for ${client.id} timed out")
      replyTo ! ResponseError(Some(id), ServiceError)
      context.stop(self)

    case OpenFileResponse(Right(OpenFileResult(buffer, capability))) =>
      replyTo ! ResponseResult(
        OpenFile,
        id,
        OpenFile
          .Result(capability, buffer.contents.toString, buffer.version)
      )
      context.stop(self)

    case OpenFileResponse(Left(failure)) =>
      replyTo ! ResponseError(
        Some(id),
        FileSystemFailureMapper.mapFailure(failure)
      )
      context.stop(self)
  }

}

object OpenFileHandler {

  def props(
    bufferRegistry: ActorRef,
    requestTimeout: FiniteDuration,
    client: Client
  ): Props = Props(new OpenFileHandler(bufferRegistry, requestTimeout, client))

}
