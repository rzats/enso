package org.enso.compiler.pass

import org.enso.compiler.core.IR

/** A representation of a compiler pass that runs on the [[IR]] type. */
trait IRPass {

  /** The type of the metadata object that the pass writes to the IR. */
  type Metadata <: IR.Metadata

  /** Executes the pass on the provided `ir`, and returns a possibly transformed
    * or annotated version of `ir`.
    *
    * Please note that a pass must return the _same_ type of IR entity that it
    * was run on.
    *
    * @param ir the Enso IR to process
    * @tparam T the input type of the IR
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  def run[T <: IR](ir: T): T
}
