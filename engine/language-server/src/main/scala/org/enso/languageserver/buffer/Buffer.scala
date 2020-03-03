package org.enso.languageserver.buffer

import org.enso.languageserver.data.buffer.Rope

/**
  * A buffer state representation.
  *
  * @param contents the contents of the buffer.
  * @param version the current version of the buffer contents.
  */
case class Buffer(contents: Rope, version: Buffer.Version)

object Buffer {
  type Version = String

  /**
    * Creates a new buffer with a freshly generated version.
    *
    * @param contents the contents of this buffer.
    * @param idGenerator a random generator for version initialization.
    * @return a new buffer instance.
    */
  def apply(contents: Rope)(implicit digest: ContentDigest): Buffer =
    Buffer(contents, digest.digest(contents.toString))

  /**
    * Creates a new buffer with a freshly generated version.
    *
    * @param contents the contents of this buffer.
    * @param idGenerator a random generator for version initialization.
    * @return a new buffer instance.
    */
  def apply(contents: String)(implicit digest: ContentDigest): Buffer =
    Buffer(Rope(contents))
}
