package org.enso.polyglot

import java.nio.ByteBuffer

import com.fasterxml.jackson.annotation.{JsonSubTypes, JsonTypeInfo}
import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.dataformat.cbor.CBORFactory
import com.fasterxml.jackson.module.scala.{DefaultScalaModule, ScalaObjectMapper}

import scala.util.Try

/**
  * A common supertype for all REPL messaging methods.
  */
@JsonTypeInfo(use = JsonTypeInfo.Id.NAME, property = "type")
@JsonSubTypes(
  Array(
    new JsonSubTypes.Type(
      value = classOf[ReplMessage.EvaluateExpressionRequest],
      name  = "evaluateExpressionRequest"
    ),
    new JsonSubTypes.Type(
      value = classOf[ReplMessage.EvaluateExpressionResponse],
      name  = "evaluateExpressionResponse"
    ),
    new JsonSubTypes.Type(
      value = classOf[ReplMessage.ListBindingsRequest],
      name  = "listBindingsRequest"
    ),
    new JsonSubTypes.Type(
      value = classOf[ReplMessage.ListBindingsResponse],
      name  = "listBindingsResponse"
    ),
    new JsonSubTypes.Type(
      value = classOf[ReplMessage.TerminateSessionRequest],
      name  = "terminateSessionRequest"
    )
  )
)
sealed trait ReplMessage

object ReplMessage {

  /**
    * A request to evaluate a given expression.
    */
  case class EvaluateExpressionRequest(expression: String) extends ReplMessage

  /**
    *  A response sent from the server upon handling a [[ListBindingsRequest]].
    *
    *  @param evaluationResult The result of evaluating the expression, in its string representation.
    */
  case class EvaluateExpressionResponse(evaluationResult: String) extends ReplMessage

  /**
    * A request to list all the bindings available in the current execution scope.
    */
  case class ListBindingsRequest() extends ReplMessage

  /**
    * A response sent from the server upon handling a [[ListBindingsRequest]].
    *
    * @param bindings A map of variable names to the current values, in their string representations.
    */
  case class ListBindingsResponse(bindings: Map[String, String]) extends ReplMessage

  /**
    * A request to terminate the REPL session.
    */
  case class TerminateSessionRequest() extends ReplMessage

  private lazy val factory = new CBORFactory()
  private lazy val mapper = {
    val mapper = new ObjectMapper(factory) with ScalaObjectMapper
    mapper.registerModule(DefaultScalaModule)
  }

  /**
    * Serializes a protocol message into a byte buffer.
    *
    * @param message the message to serialize.
    * @return the serialized version of the message.
    */
  def serialize(message: ReplMessage): ByteBuffer =
    ByteBuffer.wrap(mapper.writeValueAsBytes(message))

  /**
    * Deserializes a byte buffer into a protocol message.
    *
    * @param bytes the buffer to deserialize
    * @return the deserialized message, if the byte buffer can be deserialized.
    */
  def deserialize(bytes: ByteBuffer): Option[ReplMessage] =
    Try(mapper.readValue(bytes.array(), classOf[ReplMessage])).toOption
}
