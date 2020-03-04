package org.enso.compiler.test.pass.desugar

import org.enso.compiler.core.IR
import org.enso.compiler.test.CompilerTest
import org.enso.syntax.text.Location

class OperatorToFunctionTest extends CompilerTest {

  // === Utilities ============================================================

  /** Generates an operator and its corresponding function.
   *
   * @param name
   * @param left
   * @param right
   * @return
   */
  def genOprAndFn(
    name: IR.Name,
    left: IR.Expression,
    right: IR.Expression
  ): (IR.Application.Operator.Binary, IR.Application.Prefix) = {
    val loc = Location(1, 33)

    val binOp = IR.Application.Operator.Binary(left, name, right, Some(loc))
    val opFn  = IR.Application.Prefix(
      name,
      List(
        IR.CallArgument.Specified(None, left, left.location),
        IR.CallArgument.Specified(None, right, right.location)
      ),
      hasDefaultsSuspended = false,
      Some(loc)
    )

    (binOp, opFn)
  }

  // === The Tests ============================================================


}
