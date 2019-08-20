package org.enso.syntax.graph

import org.enso.data.List1
import org.enso.flexer.Parser.Result
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Import
import org.enso.syntax.text.Parser
import org.enso.syntax.graph.CommonAPI.Module

object Experimental {

  def expectAst(result: Parser.Result[AST.Module]): AST.Module = {
    result match {
      case Result(_, Result.Success(ret)) => ret
      case _ =>
        throw new Exception("Parsing failed: " + result.toString)
    }
  }

  def lineAsts(ast: AST.Module): List[AST] =
    ast.lines.toList.flatMap(_.elem)

  def asImport: PartialFunction[AST, Import] = { case i: Import => i }

  def collectImports(ast: AST.Module): List[AST.Import] = {
    lineAsts(ast).collect(asImport)
  }

  def prettyPrint(ast: AST.Module): Unit = {
    println("------")
    println(org.enso.syntax.text.Main.pretty(ast.toString))
    println("------")
    println(ast.show())
  }

  def parse(program: String, markers: Parser.Markers = Seq()): AST.Module = {
    val parser = new Parser()
    val result = parser.run(program, markers)
    val ast    = expectAst(result)
    parser.resolveMacros(ast)
  }

  def importedModules(ast: AST.Module): List[Module.Id] = {
    val imports: List[AST.Import] = collectImports(ast)
    imports.map(astImport => astImport.path.map(_.name))
  }
}

object Main extends App {
  val program =
    """import Foo
      |
      |import Std.Math
      |import 
      |
      |add x y = x + y
    """.stripMargin

  def playWith(input: String = program): Unit = {
    val ast = Experimental.parse(program)
    Experimental.prettyPrint(ast)

    println(Experimental.importedModules(ast).toString())
  }

  playWith(program)
}
