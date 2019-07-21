package org.enso.syntax.text

import org.enso.flexer.Macro.compile
import org.enso.flexer.Success
import org.enso.syntax.text.ast.Renamer
import org.enso.syntax.text.parser.Definition
import org.enso.flexer
import org.enso.syntax.text.AST_Mod.AST

////////////////
//// Parser ////
////////////////

class Parser {
  import Parser._
  private val engine = newEngine()

  def run(input: String): Result[AST_Mod.Module] = engine.run(input).map {
    module =>
      val module2 = module.asInstanceOf[AST_Mod.Module] // FIXME
      Renamer.run(module2)
  }

}

object Parser {
  type Result[T] = flexer.Result[T]
  private val newEngine = compile(parser.Definition)
}

//////////////
//// Main ////
//////////////

object Main extends App {

  var indent = 0

  def pprint[T](t: T) {
    if (t == null) {
      println(t)
      return
    }
    val s = t.toString()
    print("  " * indent)
    val (l, r2) = s.span(x => x != '(' && x != ')')
    print(l)
    if (r2 == "") {
      println
      return
    }

    val (m, r) = r2.splitAt(1)

    if (m == "(") {
      indent += 1
      println(m)
      pprint(r)
    } else if (m == ")") {
      indent -= 1
      println(m)
      pprint(r)
    }

  }

  val parserCons = compile(Definition)

  val p1 = new Parser()
  val p2 = new Parser()

  val out = p1.run("a + b + c")
  out match {
    case Success(v, _) =>
      pprint(v)
      println(v.show())
  }
  println()
}
