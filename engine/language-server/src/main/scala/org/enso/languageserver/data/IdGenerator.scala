package org.enso.languageserver.data
import java.util.UUID

/** Encapsulates a random IDs generator. */
trait IdGenerator {

  /** Generate a new ID for a capability registration.
    *
    * @return a fresh id.
    */
  def capabilityRegistrationId: CapabilityRegistration.Id =
    UUID.randomUUID()

  /** Generate a new buffer version.
    *
    * @return a fresh buffer version.
    */
  def bufferVersion: Buffer.Version = UUID.randomUUID()
}
