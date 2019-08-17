package org.enso.syntax.text

import org.enso.flexer
import org.enso.syntax.text
import org.enso.syntax.text.ast.meta.Builtin
import org.enso.syntax.text.AST.Marker
import org.enso.syntax.text.prec.Macro

import scala.annotation.tailrec

////////////////
//// Parser ////
////////////////

class Parser {
  import Parser._
  private val engine = newEngine()

  def run(input: String, mrkr: Seq[(Int, Marker)] = Seq()): Result[AST.Module] =
    engine.run(input, mrkr).map { module: AST =>
      val module2 = module.asInstanceOf[AST.Module] // FIXME
      Macro.run(module2)
    }

}

object Parser {
  type Result[T] = flexer.Parser.Result[T]
  private val newEngine = flexer.Parser.compile(text.ParserDef)

  def apply(): Parser = new Parser()
}

//////////////
//// Main ////
//////////////

object Main extends App {

  def pretty(str: String): String = {

    def checkClosing(in: List[Char]): Int = {
      @tailrec
      def go(i: Int, rest: Int, in: List[Char], bias: Int): Int =
        (rest, bias, in) match {
          case (0, _, _)   => 0
          case (_, 0, _)   => i
          case (_, _, Nil) => i
          case (_, _, s :: ss) =>
            s match {
              case '(' => go(i + 1, rest - 1, ss, bias - 1)
              case ')' => go(i + 1, rest - 1, ss, bias + 1)
              case _   => go(i + 1, rest - 1, ss, bias)
            }

        }
      go(0, 10, in, -1)
    }

    @tailrec
    def go(ind: Int, in: List[Char], out: List[String]): List[String] = {
      def newline(i: Int) = "\n" + " " * i * 2
      in match {
        case Nil => out
        case s :: ss =>
          val s2 = s.toString
          s match {
            case '(' =>
              checkClosing(ss) match {
                case 0 => go(ind + 1, ss, newline(ind + 1) :: s2 :: out)
                case i =>
                  go(
                    ind,
                    ss.drop(i),
                    ss.take(i).mkString("") :: s2 :: out
                  )
              }

            case ')' => go(ind - 1, ss, s2 :: newline(ind - 1) :: out)
            case ',' => go(ind, ss, newline(ind) :: s2 :: out)
            case _   => go(ind, ss, s2 :: out)
          }
      }
    }
    go(0, str.toList, List()).reverse.mkString("")
  }

  val p1 = new Parser()
  val p2 = new Parser()

  val in_def_maybe =
    """def Maybe a
      |    def Just val:a
      |    def Nothing
    """.stripMargin

  val in_arr1 = "a b -> c d"

  val inp = in_def_maybe
  val out = p1.run(inp, Seq())
  pprint.pprintln(out, width = 50, height = 10000)

  out match {
    case flexer.Parser.Result(_, flexer.Parser.Result.Success(v)) =>
      println(pretty(v.toString))

      v.lines.head.elem match {
        case Some(ast) =>
          ast match {
            case t: AST.Macro.Match =>
              println("\n---\n")
              Builtin.registry.get(t.path()) match {
                case None => println(":(")
                case Some(spec) =>
                  println("COMPUTING")
                  val out = spec.finalizer(t.segs.toList.map(_.el))
                  println(out)
              }
            case t =>
              println("not macro")
              println(t)
          }
        case _ =>
      }

      println(v.show() == inp)
      println("------")
      println(v.show())
      println("------")

  }
  println()

}
