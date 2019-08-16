package org.enso.syntax.text.precedence

import org.enso.Logger
import org.enso.data.List1
import org.enso.data.Shifted
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST._
import org.enso.syntax.text.ast.template.Builder
import org.enso.syntax.text.ast.template.Builtin

import scala.annotation.tailrec

object Template {

  val logger = new Logger()

  val Template = AST.Template

  def tokenize(ast: AST): Shifted.List1[AST] = {
    @tailrec
    def go(ast: AST, out: AST.Stream): Shifted.List1[AST] = ast match {
      case _App(fn, off, arg) => go(fn, Shifted(off, arg) :: out)
      case ast                => Shifted.List1(ast, out)
    }
    go(ast, List())
  }

  //////////////////
  //// Registry ////
  //////////////////

  def partition(t: AST): AST = {

    var builder: Builder            = Builder.moduleBuilder()
    var builderStack: List[Builder] = Nil

    def pushBuilder(ast: Ident, off: Int): Unit = logger.trace {
      builderStack +:= builder
      builder                = new Builder(ast)
      builder.current.offset = off
    }

    def popBuilder(): Unit = logger.trace {
      builder      = builderStack.head
      builderStack = builderStack.tail
    }

//    import Template._

    val root = Builder.Context(Builtin.registry.tree)

    def close(): AST.Stream1 = logger.trace {
      val revSegBldrs = List1(builder.current, builder.revSegs)
      val result = {
        builder.mixfix match {
          case None =>
            val revSegs = revSegBldrs.map { segBldr =>
              val optAst = segBldr.buildAST()
              val seg    = Template.Unmatched.Segment(segBldr.ast, optAst)
              Shifted(segBldr.offset, seg)
            }
            val segments = revSegs.reverse
            val head     = segments.head
            val tail     = segments.tail
            val paths    = builder.context.tree.dropValues()
            val stream   = Shifted.List1(head.el, tail)
            val template = Template.Unmatched(stream, paths)
            val newTok   = Shifted(head.off, template)
            List1(newTok)

          case Some(ts) =>
            val revSegTps     = ts.patterns.reverse
            val revSegsOuts   = revSegBldrs.zipWith(revSegTps)(_.build(_))
            val revSegs       = revSegsOuts.map(_._1)
            val revSegStreams = revSegsOuts.map(_._2)
            val stream        = revSegStreams.head.reverse
            val segs          = revSegs.reverse
            val shiftSegs     = Shifted.List1(segs.head.el, segs.tail)

            if (!revSegStreams.tail.forall(_.isEmpty)) {
              throw new Error(
                "Internal error: not all template segments were fully matched"
              )
            }

            val template = Template.Matched(shiftSegs)
            val newTok   = Shifted(segs.head.off, template)

            stream match {
              case Nil     => List1(newTok)
              case s :: ss => List1(s, ss) :+ newTok
            }
        }
      }
      result
    }

    def close2(): Unit = logger.trace {
      val subAst = close()
      popBuilder()
      builder.current.revBody = subAst.concat(builder.current.revBody).toList
    }

    @tailrec
    def go(input: AST.Stream): AST = {
      input match {
        case Nil =>
          if (builderStack.isEmpty) {
            logger.log("End of input (not in stack)")
            close().head.el match {
              case Template.Matched(segs) =>
                segs.head.body.toStream match {
                  case Nil    => throw new scala.Error("Impossible happened.")
                  case s :: _ => s.el
                }

              case _ => throw new scala.Error("Impossible happened.")
            }

          } else {
            logger.log("End of input (in stack)")
            close2()
            go(input)
          }
        case (t1 @ Shifted(_, el1: Ident)) :: t2_ =>
          logger.log(s"Token $t1")
          builder.context.get(el1) match {
            case Some(tr) =>
              logger.trace("New segment")
              builder.startSegment(el1, t1.off)
              builder.mixfix  = tr.value.map(Some(_)).getOrElse(builder.mixfix)
              builder.context = builder.context.copy(tree = tr)
              go(t2_)

            case None =>
              root.get(el1) match {
                case Some(tr) =>
                  logger.trace("Root")
                  val context = builder.context
                  pushBuilder(el1, t1.off)
                  builder.mixfix  = tr.value
                  builder.context = Builder.Context(tr, Some(context))
                  go(t2_)
                case None =>
                  val currentClosed = builder.context.isEmpty
                  val parentPrecWin = (builder.current.ast, el1) match {
                    case (_: Opr, _) => false
                    case (_, _: Opr) => true
                    case _           => false
                  }
                  val parentBreak = builder.context.parentCheck(el1)
                  (currentClosed || parentPrecWin) && parentBreak match {
                    case true =>
                      logger.trace("Parent close")
                      close2()
                      go(input)
                    case false =>
                      logger.trace("Add token")
                      builder.current.revBody ::= t1
                      go(t2_)
                  }
              }
          }
        case t1 :: t2_ =>
          builder.current.revBody ::= t1
          go(t2_)

      }
    }
    val elst = tokenize(t).toList()
    go(elst)
  }

  def run(module: Module): Module =
    module.map(_.map(partition))
}
