package org.enso.syntax.text.precedence

import org.enso.Logger
import org.enso.data.List1
import org.enso.data.Shifted
import org.enso.syntax.text.AST
import org.enso.syntax.text.ast.template.Builder
import org.enso.syntax.text.ast.template.Builtin

import scala.annotation.tailrec

object Template {

  val logger = new Logger()

  val Template = AST.Template

  //////////////////
  //// Registry ////
  //////////////////

  def partition(t: AST): AST = {

    var builder: Builder            = Builder.moduleBuilder()
    var builderStack: List[Builder] = Nil

    def pushBuilder(name: AST.Ident, off: Int): Unit = logger.trace {
      builderStack +:= builder
      builder = new Builder(name, off)
    }

    def popBuilder(): Option[Builder] = logger.trace {
      builderStack match {
        case Nil => None
        case b :: bs =>
          val out = builder
          builder      = b
          builderStack = bs
          Some(out)
      }
    }

    val root = Builder.Context(Builtin.registry.tree)

    def close2(): Unit = logger.trace {
      val bldr   = popBuilder().get
      val subAst = bldr.build()
      builder.current.revBody = subAst.concat(builder.current.revBody).toList
    }

    @tailrec
    def go(input: AST.Stream): AST = {
      input match {
        case Nil =>
          if (builderStack.isEmpty) {
            logger.log("End of input (not in stack)")
            builder.build().head.el match {
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
        case (t1 @ Shifted(_, el1: AST.Ident)) :: t2_ =>
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
                    case (_: AST.Opr, _) => false
                    case (_, _: AST.Opr) => true
                    case _               => false
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
    val tokens = AST.tokenize(t).toList()
    go(tokens)
  }

  def run(module: AST.Module): AST.Module =
    module.map(_.map(partition))
}
