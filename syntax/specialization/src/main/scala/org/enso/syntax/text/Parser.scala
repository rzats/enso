package org.enso.syntax.text

import org.enso.flexer
import org.enso.syntax.text
import org.enso.syntax.text.precedence.Template

////////////////
//// Parser ////
////////////////

class Parser {
  import Parser._
  private val engine = newEngine()

  def run(input: String): Result[AST.Module] = engine.run(input).map { module =>
    val module2 = module.asInstanceOf[AST.Module] // FIXME
    Template.run(module2)
  }

}

object Parser {
  type Result[T] = flexer.Parser.Result[T]
  private val newEngine = flexer.Parser.compile(text.ParserDef)

  def run(input: String): Result[AST.Module] = new Parser().run(input)
}

//////////////
//// Main ////
//////////////

object Main extends App {
  val p1 = new Parser()
  val p2 = new Parser()

  val inp = "foo  \n bar"
  val out = p1.run(inp)

  pprint.pprintln(out, width = 50, height = 10000)

  out match {
    case flexer.Parser.Result(_, flexer.Parser.Result.Success(v)) =>
      println(v.show() == inp)
      println("------")
      println(v.show().replace(' ', '.'))
      println("------")
  }
  println()
}
