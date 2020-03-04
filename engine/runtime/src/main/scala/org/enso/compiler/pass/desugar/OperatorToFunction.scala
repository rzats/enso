package org.enso.compiler.pass.desugar

import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass

/** This pass converts usages of operators to calls to standard functions. */
case class OperatorToFunction() extends IRPass {

  /** A purely desugaring pass has no analysis output. */
  override type Metadata = IR.Metadata.Empty

  /** Executes the conversion pass.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(ir: IR.Module): IR.Module = ir

  /** Executes the conversion pass in an inline context.
   *
   * @param ir the Enso IR to process
   * @return `ir`, possibly having made transformations or annotations to that
   *         IR.
   */
  override def runExpression(ir: IR.Expression): IR.Expression = ir
}
