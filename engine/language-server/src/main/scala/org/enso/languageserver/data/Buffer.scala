package org.enso.languageserver.data
import java.util.UUID

import org.enso.languageserver.data.buffer.Rope

/**
  * Represents a buffer open in the current environment.
  *
  * @param buffer the buffer contents.
  * @param clients clients holding the buffer open.
  */
case class OpenBuffer(buffer: Buffer, clients: Set[Client.Id]) {

  /**
    * Adds a new client to this buffer.
    *
    * @param client the client being added
    * @return a new buffer, with the client added.
    */
  def addClient(client: Client.Id): OpenBuffer =
    copy(clients = clients + client)
}

object OpenBuffer {

  /**
    * Creates a new open buffer with empty client list.
    *
    * @param buffer the buffer contents.
    * @return a new open buffer, with no clients.
    */
  def apply(buffer: Buffer): OpenBuffer = OpenBuffer(buffer, Set())

  /**
    * Creates a new buffer from string contents and no clients.
    *
    * @param contents the buffer contents.
    * @param idGenerator a random generator for the buffer version.
    * @return a new buffer with specified contents and no clients.
    */
  def apply(contents: String)(implicit idGenerator: IdGenerator): OpenBuffer =
    OpenBuffer(Buffer(contents))
}

/**
  * A buffer state representation.
  *
  * @param contents the contents of the buffer.
  * @param version the current version of the buffer contents.
  */
case class Buffer(contents: Rope, version: Buffer.Version)

object Buffer {
  type Version = UUID

  /**
    * Creates a new buffer with a freshly generated version.
    *
    * @param contents the contents of this buffer.
    * @param idGenerator a random generator for version initialization.
    * @return a new buffer instance.
    */
  def apply(contents: Rope)(implicit idGenerator: IdGenerator): Buffer =
    Buffer(contents, idGenerator.bufferVersion)

  /**
    * Creates a new buffer with a freshly generated version.
    *
    * @param contents the contents of this buffer.
    * @param idGenerator a random generator for version initialization.
    * @return a new buffer instance.
    */
  def apply(contents: String)(implicit idGenerator: IdGenerator): Buffer =
    Buffer(Rope(contents))
}
