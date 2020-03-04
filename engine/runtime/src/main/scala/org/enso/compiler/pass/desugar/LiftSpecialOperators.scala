package org.enso.compiler.pass.desugar

import org.enso.compiler.core.IR
import org.enso.compiler.pass.IRPass

/** This pass lifts any special operators (ones reserved by the language
  * implementation) into their own special IR constructs.
  */
case class LiftSpecialOperators() extends IRPass {

  /** A desugaring pass does not output any data. */
  override type Metadata = IR.Metadata.Empty

  /** Executes the lifting pass on a module.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runModule(ir: IR.Module): IR.Module = {
    val bindings = ir.bindings

    ir
  }

  /** Executes the lifting pass in an inline context.
    *
    * @param ir the Enso IR to process
    * @return `ir`, possibly having made transformations or annotations to that
    *         IR.
    */
  override def runExpression(ir: IR.Expression): IR.Expression = ir

}
