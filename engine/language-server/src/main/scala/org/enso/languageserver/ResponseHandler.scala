package org.enso.languageserver
import akka.actor.{Actor, ActorLogging, ActorRef, Timers}
import akka.util.Timeout
import org.enso.languageserver.jsonrpc.Errors.ServiceError
import org.enso.languageserver.jsonrpc.{
  Error,
  HasResult,
  Id,
  Method,
  Response,
  ResponseError,
  ResponseResult
}

private case object TimeoutMessage

class ResponseHandler[M <: Method, Res](
  client: ActorRef,
  method: M,
  requestId: Id,
  successHandler: PartialFunction[Any, Res],
  errorHandler: PartialFunction[Any, Error]
)(
  implicit val timeout: Timeout,
  val hasResult: HasResult.Aux[M, Res]
) extends Actor
    with Timers
    with ActorLogging {

  timers.startSingleTimer(TimeoutMessage, TimeoutMessage, timeout.duration)

  override def receive: Receive = {
    case TimeoutMessage => terminateWithServiceError()
    case Left(msg) =>
      if (errorHandler.isDefinedAt(msg)) {
        terminateWith(ResponseError(Some(requestId), errorHandler(msg)))
      } else {
        terminateWithServiceError()
      }
    case Right(msg) =>
      if (successHandler.isDefinedAt(msg)) {
        terminateWith(ResponseResult(method, requestId, successHandler(msg)))
      } else {
        terminateWithServiceError()
      }
  }

  private def terminateWith(response: Response): Unit = {
    client ! response
    context.stop(self)
  }

  private def terminateWithServiceError(): Unit =
    terminateWith(ResponseError(Some(requestId), ServiceError))
}
