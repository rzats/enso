package org.enso.syntax.text

import org.enso.flexer.Macro.compile
import org.enso.flexer.Success
import org.enso.syntax.text.ast.Renamer
import org.enso.syntax.text.parser.Definition
import org.enso.flexer
import org.enso.syntax.text.AST

import org.enso.syntax.text.ast.Renamer.MMM

////////////////
//// Parser ////
////////////////

class Parser {
  import Parser._
  private val engine = newEngine()

  def run(input: String): Result[AST.Module] = engine.run(input).map { module =>
    val module2 = module.asInstanceOf[AST.Module] // FIXME
    val ppp     = MMM.partition(module2.firstLine.elem.get)
    println("\n========= 1\n")
    pprint.pprintln(ppp, width = 50, height = 1000)
    val tt: SpacedList[AST] = SpacedList(ppp.head.el, ppp.tail)
    println("\n========= 2\n")
    pprint.pprintln(Renamer.run(tt), width = 50, height = 1000)
    //      Renamer.run(module2)
    module2
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

//  val out = p1.run("t   if  a+b   *    c     then      x")
//val out = p1.run("( (  a   )    b     c)      x")
  val out = p1.run("(a")
//  val out = p1.run("(t  if a then b)")
//  val out = p1.run("x ( a  b   )")

//  out match {
//    case Success(v, _) =>
//      pprint(v)
//      println(v.show())
//  }
  println()
}
