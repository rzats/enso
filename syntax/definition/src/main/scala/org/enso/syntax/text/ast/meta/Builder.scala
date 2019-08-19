package org.enso.syntax.text.ast.meta

import org.enso.data
import org.enso.data.List1
import org.enso.data.Shifted
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Ident
import org.enso.syntax.text.AST.Macro
import org.enso.syntax.text.AST.Macro.Ambiguous.Segment
import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.prec.Operator

import scala.annotation.tailrec

/////////////////
//// Builder ////
/////////////////

class Builder(
  head: Ident,
  offset: Int                  = 0,
  val isModuleBuilder: Boolean = false
) {
  var context: Builder.Context           = Builder.Context()
  var macroDef: Option[Macro.Definition] = None
  var current: Builder.Segment           = new Builder.Segment(head, offset)
  var revSegs: List[Builder.Segment]     = List()

  def startSegment(ast: Ident, off: Int): Unit = {
    revSegs ::= current
    current        = new Builder.Segment(ast)
    current.offset = off
  }

  def merge(that: Builder): Unit = {
    val (newRevBody, thatStream) = that.build(current.revBody)
    current.revBody = thatStream.concat(newRevBody).toList
  }

  def streamShift(off: Int, revStream: AST.Stream): (AST.Stream, Int) = {
    @tailrec
    def go(off: Int, str: AST.Stream, out: AST.Stream): (AST.Stream, Int) =
      str match {
        case Nil     => (out, off)
        case t :: ts => go(t.off, ts, Shifted(off, t.el) :: out)
      }
    val (nStream, nOff) = go(off, revStream, List())
    (nStream.reverse, nOff)
  }

  def build(revStreamL: AST.Stream): (AST.Stream, AST.Stream1) = {
    val revSegBldrs = List1(current, revSegs)
    val result = {
      macroDef match {
        case None =>
          val revSegs = revSegBldrs.map { segBldr =>
            val optAst = segBldr.buildAST()
            val seg    = Macro.Ambiguous.Segment(segBldr.ast, optAst)
            Shifted(segBldr.offset, seg)
          }
          val segments = revSegs.reverse
          val head     = segments.head
          val tail     = segments.tail
          val paths    = context.tree.dropValues()
          val stream   = Shifted.List1(head.el, tail)
          val template = Macro.Ambiguous(stream, paths)
          val newTok   = Shifted(head.off, template)
          (revStreamL, List1(newTok))

        case Some(mdef) =>
          val revSegTps     = mdef.fwdPats.reverse
          val revSegsOuts   = revSegBldrs.zipWith(revSegTps)(_.build(_))
          val revSegs       = revSegsOuts.map(_._1)
          val revSegStreams = revSegsOuts.map(_._2)
          val stream        = revSegStreams.head.reverse
          val segs          = revSegs.reverse

          val (segs2, pfxMatch, newLeftStream) = mdef.backPat match {
            case None => (segs, None, revStreamL)
            case Some(pat) =>
              val leftSeg   = new Builder.Segment(AST.Blank, 0)
              val fstSegOff = segs.head.off
              val (revStreamL2, lastStrLOff) =
                streamShift(fstSegOff, revStreamL)

              leftSeg.revBody = revStreamL2.reverse

              val (pfxSeg, revStreamL3)    = leftSeg.build(pat, reversed = true)
              val streamL3                 = revStreamL3.reverse
              val (streamL4, newFstSegOff) = streamShift(lastStrLOff, streamL3)
              val revStreamL4              = streamL4.reverse

              val newFirstSeg = segs.head.copy(off = newFstSegOff)
              val newSegs     = segs.copy(head     = newFirstSeg)

              (newSegs, Some(pfxSeg.el.body), revStreamL4)
          }

          val shiftSegs = Shifted.List1(segs2.head.el, segs2.tail)

          if (!revSegStreams.tail.forall(_.isEmpty)) {
            throw new Error(
              "Internal error: not all template segments were fully matched"
            )
          }

          val template = Macro.Match(pfxMatch, shiftSegs)
          val newTok   = Shifted(segs2.head.off, template)

          val result = stream match {
            case Nil     => List1(newTok)
            case s :: ss => List1(s, ss) :+ newTok
          }
          (newLeftStream, result)
      }
    }
    result
  }

  if (isModuleBuilder)
    macroDef = Some(
      Macro.Definition(AST.Blank -> Pattern.Expr()) { _ =>
        throw new scala.Error("Impossible happened")
      }
    )

  def buildAsModule(): AST = {
    build(List())._2.head.el match {
      case Macro.Match(_, segs) =>
        segs.head.body.toStream match {
          case Nil    => throw new scala.Error("Impossible happened.")
          case s :: _ => s.el
        }
      case _ => throw new scala.Error("Impossible happened.")
    }
  }
}

object Builder {
  def moduleBuilder(): Builder = new Builder(AST.Blank, isModuleBuilder = true)

  /////////////////
  //// Context ////
  /////////////////

  case class Context(tree: Registry.Tree, parent: Option[Context]) {
    def lookup(t: AST): Option[Registry.Tree] = tree.get(t)
    def isEmpty:        Boolean               = tree.isEmpty

    @tailrec
    final def parentLookup(t: AST): Boolean = {
      parent match {
        case None => false
        case Some(p) =>
          p.lookup(t) match {
            case None    => p.parentLookup(t)
            case Some(_) => true
          }
      }
    }
  }
  object Context {
    def apply():                    Context = Context(data.Tree(), None)
    def apply(tree: Registry.Tree): Context = Context(tree, None)
  }

  /////////////////
  //// Segment ////
  /////////////////

//  def buildMe(
//    pat: Pattern,
//    stream: AST.Stream,
//    reversed: Boolean = false
//  ): (Pattern.Match, AST.Stream) = {
//    resolve(pat, stream, reversed) match {
//      case None =>
//        throw new Error(
//          "Internal error: template pattern segment was unmatched"
//        )
//      case Some(rr) =>
//        (Shifted(offset, Match.Segment(ast, rr.elem)), rr.stream)
//    }
//  }

  class Segment(val ast: Ident, var offset: Int = 0) {
    import Macro._
    var revBody: AST.Stream = List()

    def buildAST(): Option[Shifted[AST]] = buildASTFrom(revBody.reverse)
    def buildASTFrom(stream: AST.Stream): Option[Shifted[AST]] =
      Operator.rebuild(stream)

    def build(
      pat: Pattern,
      reversed: Boolean = false
    ): (Shifted[Match.Segment], AST.Stream) = {
      val stream = revBody.reverse
      resolve(pat, stream, reversed) match {
        case None =>
          throw new Error(
            "Internal error: template pattern segment was unmatched"
          )
        case Some(rr) =>
          (Shifted(offset, Match.Segment(ast, rr.elem)), rr.stream)
      }
    }

    //////////////////////////////////////

    case class ResolveResult(elem: Pattern.Match, stream: AST.Stream) {
      def map(fn: Pattern.Match => Pattern.Match): ResolveResult =
        copy(elem = fn(elem))
    }

    def resolve(
      pattern: Pattern,
      stream: AST.Stream,
      reversed: Boolean
    ): Option[ResolveResult] = {

      def resolveList(
        p: Pattern,
        stream: AST.Stream
      ): (List[Pattern.Match], AST.Stream) = {
        @tailrec
        def go(
          stream: AST.Stream,
          revOut: List[Pattern.Match]
        ): (List[Pattern.Match], AST.Stream) =
          resolveStep(p, stream) match {
            case None    => (revOut.reverse, stream)
            case Some(t) => go(t.stream, t.elem :: revOut)
          }

        go(stream, Nil)
      }

      def resolveStep(p: Pattern, stream: AST.Stream): Option[ResolveResult] = {
        import Pattern._

        def ret[S: Repr.Of](pat: Pattern.Of[S], res: S, stream: AST.Stream) =
          Some(ResolveResult(Pattern.Match(pat, res), stream))

        p match {

          case Pattern.TillEnd(p1) =>
            resolveStep(p1, stream) match {
              case None    => None
              case Some(r) => if (r.stream.isEmpty) Some(r) else None
            }

          case p @ Pattern.Nothing() =>
            ret(p, (), stream)

          case p @ Pattern.Tag(tag, pat2) =>
            resolveStep(pat2, stream)

          case p @ Pattern.Build(pat2) =>
            resolveStep(pat2, stream).map {
              _.map { patMatch =>
                val stream  = patMatch.toStream
                val stream2 = if (reversed) stream.reverse else stream
                Pattern.Match(p, buildASTFrom(stream2).get)
              }
            }

          case p @ Pattern.Seq(pat1, pat2) =>
            resolveStep(pat1, stream) match {
              case None => None
              case Some(r1) =>
                resolveStep(pat2, r1.stream) match {
                  case None => None
                  case Some(r2) =>
                    ret(p, (r1.elem, r2.elem), r2.stream)
                }
            }

          case p @ Pattern.Cls(spaced) =>
            stream match {
              case Shifted(off, p.tag(t)) :: ss =>
                val ok = spaced match {
                  case None => true
                  case Some(s) =>
                    (s == (off > 0)) && (!t.isInstanceOf[AST.Block])
                }
                if (ok) ret(p, Shifted(off, t), ss) else None
              case _ => None
            }

          case p @ Pattern.Many(p2) =>
            val (lst, stream2) = resolveList(p2, stream)
            ret(p, lst, stream2)

          case p @ Or(p1, p2) =>
            resolveStep(p1, stream) match {
              case Some(t) => Some(t)
              case None    => resolveStep(p2, stream)
            }

          case p @ Pattern.Tok(tok, spaced) =>
            stream match {
              case Shifted(off, t) :: ss =>
                val ok = spaced.forall(_ == (off > 0))
                if (tok == t && ok) ret(p, Shifted(off, t), ss) else None
              case _ => None
            }

          case p @ Pattern.Err(msg, p1) =>
            resolveStep(p1, stream).map(
              _.map(
                x => Pattern.Match(p, Shifted(AST.Unexpected(msg, x.toStream)))
              )
            )

          case p @ Except(p1, p2) =>
            resolveStep(p1, stream) match {
              case Some(_) => None
              case None    => resolveStep(p2, stream)
            }
        }
      }
      resolveStep(pattern, stream)
    }

    override def toString: String =
      s"SegmentBuilder($offset, $revBody)"
  }
}
