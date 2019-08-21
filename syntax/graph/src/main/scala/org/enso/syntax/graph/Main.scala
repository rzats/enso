package org.enso.syntax.graph

import org.enso.syntax.text.AST

object Main extends App {

  println(AST.Number("fpo", 50).show())

  println(
    s"Show app: ${AST.App(AST.Var("inc"), AST.Number(50)).show()}"
  )
  println(
    s"Show import: ${AST.Import(AST.Cons("Foo"), AST.Cons("Bar")).show()}"
  )

  // add x y (Just z) = x + y
  // add x y (u, v) = x + y

  val program =
    """import Foo
      |
      |add x (y, z) = x + y
    """.stripMargin

  def playWith(input: String = program): Unit = {
    val ast = AstUtils.parse(program)
    AstUtils.prettyPrint(ast)

    println(s"Imported modules: ${AstUtils.importedModules(ast).toString()}")

    val defs = AstUtils.definitions(ast)
    println(s"Found ${defs.size} definitions.")
    println(defs)
    println(defs.map(_.name))
    println(defs.map(d => AstUtils.flattenDefinitionLhs(d.lhs)))
  }

  playWith(program)
}
