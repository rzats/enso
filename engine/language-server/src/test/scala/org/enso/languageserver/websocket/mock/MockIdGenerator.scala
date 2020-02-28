package org.enso.languageserver.websocket.mock
import java.util.UUID

import org.enso.languageserver.data.{
  Buffer,
  CapabilityRegistration,
  IdGenerator
}

class MockIdGenerator extends IdGenerator {
  private val len = 10

  val bufferVersions: Seq[UUID] = 0.until(len).map(_ => UUID.randomUUID())
  val capabilityRegistrations: Seq[UUID] =
    0.until(len).map(_ => UUID.randomUUID())

  private def mkInfiniteIterator(collection: Seq[UUID]): Iterator[UUID] =
    new Iterator[UUID] {
      private var current = -1

      override def hasNext: Boolean = true
      override def next(): UUID = {
        current = (current + 1) % collection.length
        collection(current)
      }
    }

  private var bufferIterator = mkInfiniteIterator(bufferVersions)
  private var capabilityRegistrationIterator = mkInfiniteIterator(
    capabilityRegistrations
  )

  override def bufferVersion: Buffer.Version = bufferIterator.next()

  override def capabilityRegistrationId: CapabilityRegistration.Id =
    capabilityRegistrationIterator.next()

  def reset(): Unit = {
    bufferIterator = mkInfiniteIterator(bufferVersions)
    capabilityRegistrationIterator = mkInfiniteIterator(
      capabilityRegistrations
    )
  }
}
