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
  def asMethod(ir: IR.Expression): IR.Module.Scope.Definition.Method = {
    IR.Module.Scope.Definition.Method("TestType", "testMethod", ir, None)
  }

  /** Hoists the provided expression as the default value of an atom argument.
    *
    * @param ir the expression to hoist
    * @return an atom with one argument `arg` with default value `ir`
    */
  def asAtomDefaultArg(ir: IR.Expression): IR.Module.Scope.Definition.Atom = {
    IR.Module.Scope.Definition.Atom(
      "TestAtom",
      List(
        IR.DefinitionArgument
          .Specified("arg", Some(ir), suspended = false, None)
      ),
      None
    )
  }

  /** Creates a module containing both an atom and a method that use the
    * provided expression.
    *
    * The expression is used in the default for an atom argument, as in
    * [[asAtomDefaultArg()]], and in the body of a method, as in [[asMethod()]].
    *
    * @param expr the expression
    * @return a module containing an atom def and method def using `expr`
    */
  def moduleDefsFrom(expr: IR.Expression): IR.Module = {
    IR.Module(List(), List(asAtomDefaultArg(expr), asMethod(expr)), None)
  }

  /** Tests whether a given operator is lifted correctly into the corresponding
    * special construct.
    *
    * @param opInfo the operator symbol
    * @param constructor the way to construct the operator
    */
  def testOperator(
    opInfo: IR.Type.Info,
    constructor: (
      IR.Expression,
      IR.Expression,
      Option[Location]
    ) => IR.Expression
  ): Unit = s"The ${opInfo.name} operator" should {
    val op    = opInfo.name
    val left  = IR.Empty(None)
    val right = IR.Empty(None)
    val loc   = Location(1, 20)

    val expressionIR = IR.Application.Operator.Binary(
      left,
      op,
      right,
      Some(loc)
    )
    val outputExpressionIR = constructor(
      left,
      right,
      Some(loc)
    )

    "be lifted by the pass in an inline context" in {
      LiftSpecialOperators()
        .runExpression(expressionIR) shouldEqual outputExpressionIR
    }

    "be lifted by the pass in a module context" in {
      val moduleInput  = moduleDefsFrom(expressionIR)
      val moduleOutput = moduleDefsFrom(outputExpressionIR)

      LiftSpecialOperators().runModule(moduleInput) shouldEqual moduleOutput
    }

    "work recursively where necessary" in {
      val recursiveIR =
        IR.Application.Operator.Binary(expressionIR, op, right, None)
      val recursiveIROutput = constructor(
        constructor(left, right, Some(loc)),
        right,
        None
      )

      LiftSpecialOperators()
        .runExpression(recursiveIR) shouldEqual recursiveIROutput
    }
  }

  // === The Tests ============================================================

  testOperator(IR.Type.Ascription, IR.Type.Ascription(_, _, _))

  testOperator(
    IR.Type.Typeset.Subsumption,
    IR.Type.Typeset.Subsumption(_, _, _)
  )

//  testOperator(IR.Type.Typeset.Equality, IR.Type.Typeset.Equality(_, _, _))
//
//  testOperator(IR.Type.Typeset.Concat, IR.Type.Typeset.Concat(_, _, _))
}
