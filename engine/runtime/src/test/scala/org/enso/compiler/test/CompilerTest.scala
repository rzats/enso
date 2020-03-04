package org.enso.compiler.test

import org.enso.compiler.codegen.AstToIR
import org.enso.compiler.core.IR
import org.enso.flexer.Reader
import org.enso.syntax.text.{AST, Parser}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

trait CompilerRunner {
  def toIR(source: String): IR = {
    val parser: Parser = Parser()
    val unresolvedAST: AST.Module =
      parser.run(new Reader(source))
    val resolvedAST: AST.Module = parser.dropMacroMeta(unresolvedAST)

    AstToIR.translate(resolvedAST)
  }
}

trait CompilerTest extends AnyWordSpecLike with Matchers with CompilerRunner
