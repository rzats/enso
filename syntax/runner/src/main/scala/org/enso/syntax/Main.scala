package org.enso.syntax
import org.enso.flexer.Macro.compile
import org.enso.flexer.Success
import org.enso.parser.Parser
import org.enso.flexer._
import org.enso.parser.AST

import scala.reflect.runtime.universe

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

////  val str = "a (b"
//  val str = "a\n b\n a" // .stripMargin
//  println(str)
//  val reader = new StringReader(str)
//  val ss     = new Lexer(reader)
//  val toks   = ss.lexAll()
//  var ttt    = 10
//  pprint(toks.toString)
//
//  val sparser = new SParser(new StringReader(str))
//
//  val bparser = new BParser(new StringReader(str))
//  val parser  = new ppp.Parser(new StringReader(str))
//
//  pprint(bparser.parse.toString())
//  pprint(parser.parse.toString())
//  pprint("!")
//  println(sparser.strInput)
//  pprint(sparser.parse.toString)
//
// '`('d`
//
// a = ('foo`bar)`baz)

  //////////////////////////////////////////////////////////////

//  @expand object Parser extends ParserBase[Int] {
//    def getResult() = Some(5)
//
//    override def initialize() = {}
//  }
//
//  println(Foo(7))

  val parserCons = compile(Parser)

  val p1 = parserCons()
  val p2 = parserCons()

  p1.bufferLen = 11

  println(p1.bufferLen)
  println(p2.bufferLen)

  val out = p1.run("a , b , c")
  out match {
    case Success(v, _) =>
      pprint(v)
      println(v.show())
      val spaceGroups = AST.Ops.partitionExprToSpaceGroups(
        v.asInstanceOf[AST.Module].firstLine.elem.get
      )
      pprint(spaceGroups)

      println("-----")
//      val out = AST.Ops.add2x(part.segs.head.expr)

      val flatExpr = spaceGroups.segs.map(s => AST.Ops.add2x(s.expr))

      println("----------------")
      val out = AST.Ops.add2x(flatExpr)
      pprint(out)
  }

//  import scala.reflect.runtime.universe._
//  val r = reify((new Foo().getClass))
//  println(r)
//
//  println(p1)
//  println(p1())
//
//  object A {
//    var x = 0;
//    def foo() = {
//      println(x)
//    }
//  }
//  (* a + b)
//  object B {
//    import A._
//    A
//  }
//
//  val b = new B()
//  println(B.a
//  val p2 = p1.run("'foo'")
//
////  val p1   = new Parser
////  val code = p1.specialize()
////  val p2   = p1.debugRun("'\\u12o45'")
//  pprint(p2)
//  p2 match {
//    case Success(v, _) =>
//      println(v.span)
//  }

//  println("CODE LEN:", code.length) //136500

  println()

}
