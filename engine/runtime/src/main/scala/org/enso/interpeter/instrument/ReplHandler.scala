package org.enso.interpeter.instrument

import java.nio.ByteBuffer

import org.enso.interpreter.instrument.ReplDebuggerInstrument
import org.enso.polyglot.ReplMessage
import org.graalvm.polyglot.io.MessageEndpoint

import scala.jdk.CollectionConverters._

/**
  * A message endpoint implementation used by the
  * [[org.enso.interpreter.instrument.ReplDebuggerInstrument]].
  *
  * @param replHandler The handler to dispatch behaviors based on received messages.
  */
class ReplEndpoint(replHandler: ReplHandler) extends MessageEndpoint {
  var client: MessageEndpoint = _

  /**
    * Sets the client end of the connection, after it has been established.
    *
    * @param ep the client endpoint.
    */
  def setClient(ep: MessageEndpoint): Unit = client = ep

  /**
    * Sends a message to the connected client.
    *
    * @param msg the message to send.
    */
  def sendToClient(msg: ReplMessage): Unit =
    client.sendBinary(ReplMessage.serialize(msg))

  override def sendText(text: String): Unit = {}

  override def sendBinary(data: ByteBuffer): Unit =
    ReplMessage.deserialize(data).foreach(replHandler.onMessage)

  override def sendPing(data: ByteBuffer): Unit = client.sendPong(data)

  override def sendPong(data: ByteBuffer): Unit = {}

  override def sendClose(): Unit = client.sendClose()
}

/**
  * A message handler, dispatching behaviors based on messages received
  * from an instance of [[ReplEndpoint]].
  */
class ReplHandler {
  val replEndpoint                                                 = new ReplEndpoint(this)
  var executionNode: ReplDebuggerInstrument.ReplExecutionEventNode = _

  /**
    * Sets the execution event node to be used for message callbacks.
    *
    * @param node the execution event node
    */
  def setExecutionNode(
    node: ReplDebuggerInstrument.ReplExecutionEventNode
  ): Unit = executionNode = node

  /**
    * Handles a message received from the client.
    *
    * @param msg the message to handle
    */
  def onMessage(msg: ReplMessage): Unit = msg match {
    case ReplMessage.EvaluateExpressionRequest(expression) =>
      val evaluationResult = executionNode.evaluate(expression).toString
      replEndpoint.sendToClient(
        ReplMessage.EvaluateExpressionResponse(evaluationResult)
      )
    case ReplMessage.ListBindingsRequest() =>
      val bindings = executionNode
        .listBindings()
        .asScala
        .view
        .mapValues(_.toString)
        .toMap
      replEndpoint.sendToClient(ReplMessage.ListBindingsResponse(bindings))
    case ReplMessage.TerminateSessionRequest() =>
      executionNode.exit()
      replEndpoint.sendClose()
  }
}
