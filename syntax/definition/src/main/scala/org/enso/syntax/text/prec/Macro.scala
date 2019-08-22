package org.enso.syntax.text.prec

import org.enso.Logger
import org.enso.data.Shifted
import org.enso.syntax.text.AST
import org.enso.syntax.text.ast.meta.Builder
import org.enso.syntax.text.ast.meta.Builtin

import scala.annotation.tailrec

object Macro {
  val logger = new Logger()

  //////////////////
  //// Registry ////
  //////////////////

  def run(module: AST.Module): AST.Module =
    module.map(transform)

  private def transform(t: AST): AST = {
    val root                        = Builder.Context(Builtin.registry.tree)
    var builder: Builder            = Builder.moduleBuilder()
    var builderStack: List[Builder] = Nil

    def pushBuilder(name: AST.Ident, off: Int, lineBegin: Boolean): Unit =
      logger.trace {
        builderStack +:= builder
        builder = new Builder(name, off, lineBegin)
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

    def isRoot(): Boolean =
      builderStack.isEmpty

    var isLineBegin: Boolean = true

    @tailrec
    def finalize(): AST = {
      popBuilder() match {
        case Some(bldr) =>
          logger.log("End of input (in stack)")
          println(bldr.current.stream)
          println(builder.current.stream)
          println(builder.revSegs)
//          val rstream = builder.mergex(bldr)
//          go(rstream)
          builder.merge(bldr)
          finalize()
        case None =>
          logger.log("End of input (not in stack)")
          builder.buildAsModule()
      }
    }
    @tailrec
    def go(input: AST.Stream): AST = {
      input match {
        case Nil =>
          val builders = (builder :: builderStack)
//
          var newRevBuilders: List[Builder] = List()
//
          var subStream: AST.Stream = List()
          for (b <- builders) {
            println("")
            println("-----------")
            println(b.revSegs)
            println(b.current.stream)
            val noLastPattern = b.macroDef.map(_.last.pattern) == Some(None)
//            println(b.revSegs)
            if (noLastPattern) {
              println(">>>")
              val (revLeftUnusedStream, matched, rightUnusedStream) =
                b.build(List())
//              revOutStream = rightUnusedStream ++ (matched :: revLeftUnusedStream)
              println(s"matched:")
              pprint.pprintln(matched, width = 50, height = 10000)
              println(s"revLeftUnusedStream:")
              pprint.pprintln(revLeftUnusedStream, width = 50, height = 10000)
              println(s"rightUnusedStream:")
              pprint.pprintln(rightUnusedStream, width = 50, height = 10000)
              subStream = subStream ++ revLeftUnusedStream ++ (matched :: rightUnusedStream)
            } else {
              println("~~~~~~~~~~~~~~")
              println(b.current.stream)
              b.current.stream ++= subStream
              subStream = List()
              println(b.current.stream)
              newRevBuilders +:= b
            }
//
//            println(s"OUTSTREAM:")
//            pprint.pprintln(revOutStream, width = 50, height = 10000)
//
//            println("")
          }

          val newBuilders = newRevBuilders.reverse

          builder      = newBuilders.head
          builderStack = newBuilders.tail
////          println("---------------------------------")
////          println("---------------------------------")
////          println("---------------------------------")
////          pprint.pprintln(revOutStream, width = 50, height = 10000)
//
//          builder = builders.head
//          builder.current.stream ++= revOutStream.reverse
//
//          builder.buildAsModule()
          finalize()
        case (t1 @ Shifted(off, el1: AST.Ident)) :: t2_ =>
          logger.log(s"Token $t1")
          logger.beginGroup()
          val wasLineBegin = isLineBegin
          isLineBegin = false
          builder.context.lookup(el1) match {
            case Some(tr) =>
              logger.log("New segment")
              builder.beginSegment(el1, off)
              builder.macroDef =
                tr.value.map(Some(_)).getOrElse(builder.macroDef)
              builder.context = builder.context.copy(tree = tr)
              val knownMacro = builder.context.isEmpty
//              if (false) { // knownMacro
//                builder.current.stream = t2_
//                popBuilder() match {
//                  case Some(bldr) =>
//                    logger.log("Building known macro")
//                    val newStream = builder.mergex(bldr)
//                    go(newStream)
//                  case _ => ??? // impossible, module dos not have segments
//                }
//              } else {
              logger.endGroup()
              go(t2_)
//              }

            case None =>
              root.lookup(el1) match {
                case Some(tr) =>
                  logger.log("New macro")
                  val context = builder.context
                  pushBuilder(el1, t1.off, wasLineBegin)
                  builder.macroDef = tr.value
                  builder.context  = Builder.Context(tr, Some(context))
                  logger.endGroup()
                  go(t2_)

                case _ =>
                  val currentClosed = builder.context.isEmpty
                  val parentPrecWin = (builder.current.ast, el1) match {
                    case (_: AST.Opr, _) => false
                    case (_, _: AST.Opr) => true
                    case _               => false
                  }
                  val parentBreak = builder.context.parentLookup(el1)
                  (currentClosed || parentPrecWin) && parentBreak match {
                    case true =>
                      logger.log("Parent close")
                      val subBuilder = builder
                      popBuilder()
                      builder.merge(subBuilder)
                      logger.endGroup()
                      go(input)
                    case false =>
                      logger.log("Add token")
                      // FIXME: slow
                      builder.current.stream = builder.current.stream ++ List(
                          t1
                        )
                      logger.endGroup()
                      go(t2_)
                  }
              }
          }
        case (t1 @ Shifted(off, el1: AST.Block)) :: t2_ =>
          val nt1 = Shifted(off, el1.map(transform))
          // FIXME: slow
          builder.current.stream = builder.current.stream ++ List(nt1)
          go(t2_)

        case t1 :: t2_ =>
          // FIXME: slow
          builder.current.stream = builder.current.stream ++ List(t1)
          go(t2_)

      }
    }
    val stream = AST.tokenize(t).toList()
    go(stream)
//    val stream = AST.tokenize(t)
//    val ss     = List1(Shifted(stream.head), stream.tail)
//    val ss2    = Operator.rebuildNonSpaced(ss)
//    go(stream)
  }
}
