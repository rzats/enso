package org.enso.syntax.graph

import org.enso.syntax.text.AST
import org.enso.syntax.text.ast.DSL._

object Main extends App {

  println(AST.Number("fpo", 50).show())

  println(
    s"Show app: ${("inc" $_ AST.Number(50)).show()}"
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
    val ast = ParserUtils.parse(input)
    ParserUtils.prettyPrint(ast)
  }

  playWith(program)
}
