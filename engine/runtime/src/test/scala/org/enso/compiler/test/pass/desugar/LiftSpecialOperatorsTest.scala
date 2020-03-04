package org.enso.compiler.test.pass.desugar

import org.enso.compiler.core.IR
import org.enso.compiler.pass.desugar.LiftSpecialOperators
import org.enso.compiler.test.CompilerTest
import org.enso.syntax.text.Location

class LiftSpecialOperatorsTest extends CompilerTest {

  "The type ascription operator" should {
    "be lifted by the pass" in {
      val left = IR.Empty(None)
      val right = IR.Empty(None)
      val loc = Location(1, 20)

      val inputIR = IR.Application.Operator.Binary(
        left,
        ":",
        right,
        Some(loc)
      )

      val outputIR = IR.Type.Ascription(
        left,
        right,
        Some(loc)
      )

//      LiftSpecialOperators().run(inputIR) shouldEqual outputIR
    }
  }
}
