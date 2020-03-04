package org.enso.compiler.test.pass.desugar

import org.enso.compiler.core.IR
import org.enso.compiler.pass.desugar.LiftSpecialOperators
import org.enso.compiler.test.CompilerTest
import org.enso.syntax.text.Location

class LiftSpecialOperatorsTest extends CompilerTest {

  // === Utilities ============================================================

  /** Hoists the provided expression into the body of a method.
    *
    * @param ir the expression to hoist
    * @return a method containing `ir` as its body
    */
  def asMethod(ir: IR.Expression): IR.ModuleScope.Definition.Method = {
    IR.ModuleScope.Definition.Method("TestType", "testMethod", ir, None)
  }

  /** Hoists the provided expression as the default value of an atom argument.
    *
    * @param ir the expression to hoist
    * @return an atom with one argument `arg` with default value `ir`
    */
  def asAtomDefaultArg(ir: IR.Expression): IR.ModuleScope.Definition.Atom = {
    IR.ModuleScope.Definition.Atom(
      "TestAtom",
      List(
        IR.DefinitionArgument
          .Specified("arg", Some(ir), suspended = false, None)
      ),
      None
    )
  }

  // === The Tests ============================================================

  "The type ascription operator" should {
    val left  = IR.Empty(None)
    val right = IR.Empty(None)
    val loc   = Location(1, 20)

    val expressionIR = IR.Application.Operator.Binary(
      left,
      ":",
      right,
      Some(loc)
    )

    "be lifted by the pass in an inline context" in {
      val outputIR = IR.Type.Ascription(
        left,
        right,
        Some(loc)
      )

//      LiftSpecialOperators().runExpression(expressionIR) shouldEqual outputIR
    }

    "be lifted by the pass in a module context" in {}
  }
}
