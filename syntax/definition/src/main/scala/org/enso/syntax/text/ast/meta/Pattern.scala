package org.enso.syntax.text.ast.meta

import org.enso.data.Shifted
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.SAST
import org.enso.syntax.text.AST.Stream
import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.meta.Pattern.Of
import org.enso.syntax.text.prec.Operator

import scala.annotation.tailrec
import scala.reflect.ClassTag
import org.enso.data.Shifted

import cats.implicits._

/////////////////
//// Pattern ////
/////////////////

sealed trait Pattern {
  import Pattern._
  def ::(that: Pattern): Seq     = Seq(that, this)
  def !(that: Pattern):  Except  = Except(that, this)
  def |(that: Pattern):  Or      = Or(this, that)
  def |?(tag: String):   Tag     = Tag(tag, this)
  def or(that: Pattern): Or      = Or(this, that)
  def or(msg: String):   Or      = this.or(Err(msg))
  def many:              Many    = Many(this)
  def many1:             Pattern = Many1(this)
  def tag(tag: String):  Tag     = Tag(tag, this)
  def opt:               Or      = this | Nothing()
  def build:             Build   = Build(this)

  def match_(
    stream: Stream,
    lineBegin: Boolean = false,
    reversed: Boolean  = false
  ): MatchResult =
    Pattern.matchUnsafe(this, stream, lineBegin, reversed)

  def matchRev(stream: Stream, lineBegin: Boolean = false): MatchResult =
    this.match_(stream, lineBegin = lineBegin, reversed = true)

  def matchOpt(
    stream: Stream,
    lineBegin: Boolean = false,
    reversed: Boolean  = false
  ): Option[MatchResult] =
    Pattern.matchOpt(this, stream, lineBegin, reversed)
}

//object Pattern2 {
//  type True  = True.type
//  type False = False.type
//
//  sealed trait Bool {
//    type &&[B <: Bool] <: Bool
//    type ||[B <: Bool] <: Bool
//    type IfElse[T, F] <: Any
//    type ?[T] <: Any
//  }
//  object True extends Bool {
//    type &&[B <: Bool] = B
//    type ||[B <: Bool] = True
//    type IfElse[T, F] = T
//    type ?[T]         = T
//  }
//  object False extends Bool {
//    type &&[B <: Bool] = False
//    type ||[B <: Bool] = B
//    type IfElse[T, F] = F
//    type ?[T]         = Unit
//  }
//
//  implicitly[(False# &&[False]) =:= False]
//
//  type If[T <: Bool, A] = T#IfElse[A, Unit]
//
//  val a: True# &&[True] = True
//
//  val b: If[True, Int]  = 7
//  val c: If[False, Int] = "test"
//
//  trait Pattern
//  trait PatternOf[T] extends Pattern
//  type P[T] = PatternOf[T]
////  type PT   = P[True]
//  type PF = P[False]
//
//  type Choice[T] = Either[P[T], P[T]]
//
//  case class Empty[T]()                                         extends P[T]
//  case class Tok[T](tok: AST, spaced: Spaced, ast: If[T, SAST]) extends P[T]
//  case class Many[T](pat: PF, elem: If[T, List[P[T]]])          extends P[T]
//  case class Seq[T](first: P[T], second: P[T])                  extends P[T]
//  case class Build[T](pat: P[T], ast: If[T, SAST])              extends P[T]
//  case class Except[T](not: PF, pat: P[T])                      extends P[T]
//  case class Or[T](left: PF, right: PF, ast: If[T, Choice[T]])  extends P[T]
//  //  final case class Or(p1: Pattern, p2: Pattern)      extends Of[Match]
//  //  final case class TillEnd(pat: Pattern)             extends Of[Match]
////  final case class FromBegin(pat: Pattern)           extends Of[Match]
////  final case class Tag(tag: String, pat: Pattern)    extends Of[Match]
////  final case class Err(msg: String, pat: Pattern)    extends Of[SAST]
////  final case class ClsOpr(maxPrec: Option[Int])      extends Of[SAST]
////  final case class Cls[T <: AST](spaced: Option[Boolean])(implicit val tag: ClassTag[T])
////      extends Of[Shifted[T]]
//}

trait Pattern3
object Pattern3 {
  type P      = Pattern3
  type Spaced = Option[Boolean]

  trait Class
  object Class {
    final case object Normal  extends Class
    final case object Pattern extends Class
  }

  // format: off
  /** Boundary Patterns */
  final case class Begin   ()                                       extends P
  final case class End     ()                                       extends P

  /** Structural Patterns */
  final case class Nothing ()                                       extends P
  final case class Seq     (pat1 : P     , pat2   : P)              extends P
  final case class Or      (pat1 : P     , pat2   : P)              extends P
  final case class Many    (pat  : P)                               extends P
  final case class Except  (not  : P     , pat    : P)              extends P
  
  /** Meta Patterns */
  final case class Build   (pat  : P)                               extends P
  final case class Err     (msg  : String, pat    : P)              extends P
  final case class Tag     (tag  : String, pat    : P)              extends P
  final case class Cls     (cls  : Class , pat    : P)              extends P

  /** Token Patterns */
  final case class Tok     (spaced : Spaced, ast  : AST)            extends P
  final case class Var     (spaced : Spaced)                        extends P
  final case class Cons    (spaced : Spaced)                        extends P
  final case class Opr     (spaced : Spaced, maxPrec: Option[Int])  extends P
  final case class Num     (spaced : Spaced)                        extends P
  final case class Text    (spaced : Spaced)                        extends P
  final case class Block   (spaced : Spaced)                        extends P
  // format: on
}

object Match3 {
  type M = Match3
  type P = Pattern3
  val P = Pattern3
  val A = AST

  // format: off
  /** Boundary Matches */
  final case class Begin   (pat : P.Begin)                           extends M
  final case class End     (pat : P.End)                             extends M

  /** Structural Matches */
  final case class Nothing (pat : P.Nothing)                         extends M
  final case class Seq     (pat : P.Seq      , elems : (M, M))       extends M
  final case class Or      (pat : P.Or       , elem  : Either[M,M])  extends M
  final case class Many    (pat : P.Many     , elems : List[M])      extends M
  final case class Except  (pat : P.Except   , elem  : M)            extends M

  /** Meta Matches */
  final case class Build   (pat : P.Build    , ast   : SAST)         extends M
  final case class Err     (pat : P.Err      , ast   : SAST)         extends M
  final case class Tag     (pat : P.Tag      , elem  : M)            extends M
  final case class Cls     (pat : P.Cls      , elem  : M)            extends M

  /** Token Matches */
  final case class Tok     (pat : P.Tok   , ast : SAST)              extends M
  final case class Var     (pat : P.Var   , ast : Shifted[A.Var])    extends M
  final case class Cons    (pat : P.Cons  , ast : Shifted[A.Cons])   extends M
  final case class Opr     (pat : P.Opr   , ast : Shifted[A.Opr])    extends M
  final case class Num     (pat : P.Num   , ast : Shifted[A.Number]) extends M
  final case class Text    (pat : P.Text  , ast : Shifted[A.Text])   extends M
  final case class Block   (pat : P.Block , ast : Shifted[A.Block])  extends M
  // format: on
}
trait Match3 {
  import Match3._
  val pat: Pattern3

  def toStream: Stream = this match {
    case _: Begin   => List()
    case _: End     => List()
    case _: Nothing => List()
    case m: Seq     => m.elems._1.toStream ++ m.elems._2.toStream
    case m: Or      => m.elem.merge.toStream
    case m: Many    => m.elems.flatMap(_.toStream)
    case m: Except  => m.elem.toStream
    case m: Build   => List(m.ast)
    case m: Err     => List(m.ast)
    case m: Tag     => m.elem.toStream
    case m: Cls     => m.elem.toStream
    case m: Tok     => List(m.ast)
    case m: Var     => List(m.ast)
    case m: Cons    => List(m.ast)
    case m: Opr     => List(m.ast)
    case m: Num     => List(m.ast)
    case m: Text    => List(m.ast)
    case m: Block   => List(m.ast)
  }
}

object Pattern {
  sealed trait Of[T] extends Pattern
  type Spaced = Option[Boolean]

  final case class Nothing()                         extends Of[Unit]
  final case class Tok(tok: AST, spaced: Spaced)     extends Of[SAST]
  final case class Many(pat: Pattern)                extends Of[List[Match]]
  final case class Seq(p1: Pattern, p2: Pattern)     extends Of[(Match, Match)]
  final case class Build(pat: Pattern)               extends Of[SAST]
  final case class Except(not: Pattern, ok: Pattern) extends Of[Match]
  final case class Or(p1: Pattern, p2: Pattern)      extends Of[Match]
  final case class TillEnd(pat: Pattern)             extends Of[Match]
  final case class FromBegin(pat: Pattern)           extends Of[Match]
  final case class Tag(tag: String, pat: Pattern)    extends Of[Match]
  final case class Err(msg: String, pat: Pattern)    extends Of[SAST]
  final case class ClsOpr(maxPrec: Option[Int])      extends Of[SAST]
  final case class Cls[T <: AST](spaced: Spaced)(implicit val tag: ClassTag[T])
      extends Of[Shifted[T]]

  object Tok {
    def apply(tok: AST): Tok = Tok(tok, None)
  }

  object Cls {
    def apply[T <: AST: ClassTag](): Cls[T] = new Cls[T](None)
  }

  object Seq {
    def apply(p1: Pattern, p2: Pattern, ps: Pattern*): Pattern =
      ps.headOption match {
        case None     => Seq(p1, p2)
        case Some(p3) => Seq(Seq(p1, p2), p3, ps.tail: _*)
      }
  }

  object Err {
    def apply(msg: String): Err = Err(msg, Nothing())
  }

  object TillEnd {
    def apply(): TillEnd = TillEnd(Nothing())
  }

  object FromBegin {
    def apply(): FromBegin = FromBegin(Nothing())
  }

  //// Conversions ////

  implicit def fromAST(ast: AST): Pattern = Tok(ast)

  //// Smart Constructors ////

  object Opt {
    def apply(pat: Pattern) = pat | Nothing()
    def unapply(t: Pattern): Option[Pattern] = t match {
      case Or(pat, Nothing()) => Some(pat)
      case _                  => None
    }
  }

  object Any {
    def apply(spaced: Option[Boolean] = None): Pattern = Cls[AST](spaced)
    def unapply(t: Pattern)(implicit astCls: ClassTag[AST]): Boolean =
      t match {
        case t @ Cls(None) => t.tag == astCls
        case _             => false
      }
  }

  object NonSpacedAny {
    def apply(): Pattern = Cls[AST](Some(false))
    def unapply(t: Pattern)(implicit astCls: ClassTag[AST]): Boolean =
      t match {
        case t @ Cls(Some(false)) => t.tag == astCls
        case _                    => false
      }
  }

  object End {
    def apply(): TillEnd = TillEnd(Nothing())
    def unapply(t: Pattern): Boolean =
      t match {
        case TillEnd(Nothing()) => true
        case _                  => false
      }
  }

  object Not {
    def apply(pat: Pattern): Pattern = Except(pat, Nothing())
    def unapply(t: Pattern): Option[Pattern] = t match {
      case Except(pat, Nothing()) => Some(pat)
      case _                      => None
    }
  }

  object AnyBut {
    def apply(pat: Pattern): Pattern = Any() ! pat
    def unapply(t: Pattern): Option[Pattern] = t match {
      case Except(pat, Any()) => Some(pat)
      case _                  => None
    }
  }

  // FIXME: check unapply
  object Many1 {
    def apply(pat: Pattern): Seq = Seq(pat, Many(pat))
    def unapply(p: Seq): Option[Pattern] = p match {
      case Seq(p1, Many(p2)) => if (p == p1) Some(p) else None
    }
  }

  object AnyTill {
    def apply(pat: Pattern): Many = Many(AnyBut(pat))
  }

  object TillEndMarkUnmatched {
    def apply(pat: Pattern, msg: String) =
      TillEnd(pat) | (pat :: ErrTillEnd(msg))
  }

  object Expr {
    def apply() = Build(Many1(Any()))
    def unapply(t: Pattern): Boolean = t match {
      case Build(Many1(Any())) => true
      case _                   => false
    }
  }

  object OprExpr {
    def apply(opr: String) = {
      val base = Except(ClsOpr(Some(AST.Opr(opr).prec)), Any())
      base.many1.build
    }
  }

  object NonSpacedExpr {
    def apply() = Any(Some(false)).many1.build
  }

  object NonSpacedExpr_ {
    def apply() = Build(AnyBut(Cls[AST.Block]) :: Many(NonSpacedAny()))
  }

  object SepList {
    def apply(pat: Pattern, div: Pattern): Seq = pat :: (div :: pat).many
    def apply(pat: Pattern, div: Pattern, err: String): Seq = {
      val seg = pat | Err(err, AnyTill(div))
      SepList(seg, div)
    }
  }

  object AnyTillEnd {
    def apply(): Many = Many(Any())
  }

  object ErrTillEnd {
    def apply(msg: String): Err = Err(msg, AnyTillEnd())
  }

  case class MatchResult(elem: Pattern.Match, stream: Stream) {
    def map(fn: Pattern.Match => Pattern.Match): MatchResult =
      copy(elem = fn(elem))
  }

  case class MatchResult3(elem: Match3, stream: Stream) {
    def map(fn: Match3 => Match3): MatchResult3 =
      copy(elem = fn(elem))
  }

  def buildASTFrom(stream: Stream): Option[Shifted[AST]] =
    Operator.rebuild(stream)

  ///////////////
  //// Match ////
  ///////////////

  /** Result of AST tokens Macro pattern match. */
  type Match = Match.Of[_]
  object Match {
    def apply[T: Repr.Of](pat: Pattern.Of[T], el: T): Match.Of[T] =
      Match.Of(pat, el)

    final case class Of[T: Repr.Of](pat: Pattern.Of[T], el: T)
        extends Repr.Provider {
      val repr = Repr.of(el)

      override def toString =
        s"${pat.getClass.getSimpleName}(${el.toString})"

      def toStream: Stream = this match {
        case Match.Build(t)     => List(t)
        case Match.Cls(t)       => List(t)
        case Match.Err(t)       => List(t)
        case Match.FromBegin(t) => t.toStream
        case Match.Many(t)      => t.flatMap(_.toStream)
        case Match.Except(t)    => t.toStream
        case Match.Nothing()    => List()
        case Match.Or(t)        => t.toStream
        case Match.Seq(l, r)    => l.toStream ++ r.toStream
        case Match.Tag(tag, t)  => t.toStream
        case Match.TillEnd(t)   => t.toStream
        case Match.Tok(t)       => List(t)
      }

      def isValid: Boolean = this match {
        case Match.Build(_)     => true
        case Match.Cls(_)       => true
        case Match.Err(_)       => false
        case Match.FromBegin(t) => t.isValid
        case Match.Many(t)      => t.forall(_.isValid)
        case Match.Except(t)    => t.isValid
        case Match.Nothing()    => true
        case Match.Or(t)        => t.isValid
        case Match.Seq(l, r)    => l.isValid && r.isValid
        case Match.Tag(_, t)    => t.isValid
        case Match.TillEnd(t)   => t.isValid
        case Match.Tok(_)       => true
      }

      def map(f: AST => AST): Of[T] = mapX(this)(f)

      def mapList[A](t: List[A])(f: A => A): List[A] = t.map(f)

      def mapY(t: Match.Of[_])(f: AST => AST): Match.Of[_] = mapX(t)(f)
      def mapX[X](t: Match.Of[X])(f: AST => AST): Match.Of[X] = t match {
        case Of(pat: Pattern.Build, s)     => Of[X](pat, s.map(f))
        case Of(pat: Pattern.Cls[AST], s)  => Of[X](pat, s.map(f))
        case Of(pat: Pattern.Err, s)       => Of[X](pat, s.map(f))
        case Of(pat: Pattern.FromBegin, s) => Of[X](pat, mapY(s)(f))
        case Of(pat: Pattern.Many, s) =>
          Of[X](pat, mapList(s)((m: Match) => (m: Match)))
        case Of(pat: Pattern.Except, s)  => Of[X](pat, mapY(s)(f))
        case Of(pat: Pattern.Nothing, s) => Of[X](pat, s)
        case Of(pat: Pattern.Or, s)      => Of[X](pat, mapY(s)(f))
        case Of(pat: Pattern.Seq, (s1, s2)) =>
          Of[X](pat, (mapY(s1)(f), mapY(s2)(f)))
        case Of(pat: Pattern.Tag, s)     => Of[X](pat, mapY(s)(f))
        case Of(pat: Pattern.TillEnd, s) => Of[X](pat, mapY(s)(f))
        case Of(pat: Pattern.Tok, s)     => Of[X](pat, s.map(f))

      }
    }

    //// Smart Deconstructors ////

    object Build {
      def unapply(t: Match): Option[SAST] = t match {
        case Of(_: Pattern.Build, t) => Some(t)
        case _                       => None
      }
    }

    object Cls {
      def unapply(t: Match): Option[SAST] = t match {
        case Of(_: Pattern.Cls[_], t) => Some(t)
        case _                        => None
      }
    }

    object Err {
      def unapply(t: Match): Option[SAST] = t match {
        case Of(_: Pattern.Err, t) => Some(t)
        case _                     => None
      }
    }

    object Many {
      def unapply(t: Match): Option[List[Match]] = t match {
        case Of(_: Pattern.Many, t) => Some(t)
        case _                      => None
      }
    }

    object Except {
      def unapply(t: Match): Option[Match] = t match {
        case Of(_: Pattern.Except, s) => Some(s)
        case _                        => None
      }
    }

    object FromBegin {
      def unapply(t: Match): Option[Match] = t match {
        case Of(_: Pattern.FromBegin, t) => Some(t)
        case _                           => None
      }
    }

    object Nothing {
      def apply() = Match.Of(Pattern.Nothing(), ())
      def unapply(t: Match): Boolean = t match {
        case Of(_: Pattern.Nothing, _) => true
        case _                         => false
      }
    }

    object Or {
      def unapply(t: Match): Option[Match] = t match {
        case Of(_: Pattern.Or, t) => Some(t)
        case _                    => None
      }
    }

    object Seq {
      def apply(p1: Match, p2: Match): Match =
        Match(Pattern.Seq(p1.pat, p2.pat), (p1, p2))
      def unapply(t: Match): Option[(Match, Match)] = t match {
        case Of(_: Pattern.Seq, t) => Some(t)
        case _                     => None
      }
    }

    object Tag {
      def unapply(t: Match): Option[(String, Match)] = t match {
        case Of(tag: Pattern.Tag, t) => Some((tag.tag, t))
        case _                       => None
      }
    }

    object TillEnd {
      def unapply(t: Match): Option[Match] = t match {
        case Of(_: Pattern.TillEnd, t) => Some(t)
        case _                         => None
      }
    }

    object Tok {
      def unapply(t: Match): Option[SAST] = t match {
        case Of(_: Pattern.Tok, t) => Some(t)
        case _                     => None
      }
    }
  }

  //////////////////////////////////
  //// Pattern Match Resolution ////
  //////////////////////////////////

  def streamShift_(off: Int, revStream: AST.Stream): AST.Stream =
    streamShift(off, revStream)._1

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

  /** Unsafe variant of AST Macro tokens pattern matching. If you want to use
    * patterns that could not match all input tokens, use [[matchOpt]] instead.
    */
  def matchUnsafe(
    pattern: Pattern,
    stream: Stream,
    lineBegin: Boolean = false,
    reversed: Boolean  = false
  ): MatchResult = {
    matchOpt(pattern, stream, lineBegin, reversed).getOrElse {
      val msg = "Internal error: template pattern segment was unmatched"
      throw new Error(msg)
    }
  }

  /** This function takes a pattern and applies it to AST input stream. The
    * optional parameter 'reversed' is used for prefix (reverse) matching and is
    * used for prefix macro matching. The function assumes that the pattern does
    * not fail.
    */
  def matchOpt(
    pattern: Pattern,
    stream: Stream,
    lineBegin: Boolean,
    reversed: Boolean
  ): Option[MatchResult] = {
    def matchList(p: Pattern, stream: Stream): (List[Match], Stream) = {
      @tailrec
      def go(stream: Stream, revOut: List[Match]): (List[Match], Stream) =
        matchStep(p, stream) match {
          case None    => (revOut.reverse, stream)
          case Some(t) => go(t.stream, t.elem :: revOut)
        }
      go(stream, Nil)
    }

    def matchStep(p: Pattern, stream: Stream): Option[MatchResult] = {

      def ret[S: Repr.Of](pat: Pattern.Of[S], res: S, stream: Stream) =
        Some(MatchResult(Pattern.Match(pat, res), stream))

      p match {

        case Pattern.TillEnd(p1) =>
          matchStep(p1, stream) match {
            case None    => None
            case Some(r) => if (r.stream.isEmpty) Some(r) else None
          }

        case Pattern.FromBegin(p1) =>
          if (lineBegin) matchStep(p1, stream) else None

        case p @ Pattern.Nothing() =>
          ret(p, (), stream)

        case p @ Pattern.Tag(tag, pat2) =>
          matchStep(pat2, stream)

        case p @ Pattern.Build(pat2) =>
          matchStep(pat2, stream).map {
            _.map { patMatch =>
              val stream = patMatch.toStream
              val ast =
                if (!reversed) buildASTFrom(stream).get
                else {
                  // When performing reverse pattern match, tokens use
                  // right-offsets instead of left ones, so we need to push them
                  // back before computing AST.
                  val (shiftedStream, off) = streamShift(0, stream.reverse)
                  val shiftedAst           = buildASTFrom(shiftedStream).get
                  shiftedAst.copy(off = off)
                }
              Pattern.Match(p, ast)
            }
          }

        case p @ Pattern.Seq(pat1, pat2) =>
          matchStep(pat1, stream) match {
            case None => None
            case Some(r1) =>
              matchStep(pat2, r1.stream) match {
                case None => None
                case Some(r2) =>
                  ret(p, (r1.elem, r2.elem), r2.stream)
              }
          }

        case p @ Pattern.ClsOpr(maxPrec) =>
          stream match {
            case Shifted(off, t: AST.Opr) :: ss => {
              val isOk = maxPrec match {
                case None       => true
                case Some(prec) => prec >= t.prec
              }
              if (isOk) ret(p, Shifted(off, t), ss) else None
            }
            case _ => None
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
          val (lst, stream2) = matchList(p2, stream)
          ret(p, lst, stream2)

        case p @ Or(p1, p2) =>
          matchStep(p1, stream) match {
            case Some(t) => Some(t)
            case None    => matchStep(p2, stream)
          }

        case p @ Pattern.Tok(tok, spaced) =>
          stream match {
            case Shifted(off, t) :: ss =>
              val ok = spaced.forall(_ == (off > 0))
              if (tok == t && ok) ret(p, Shifted(off, t), ss) else None
            case _ => None
          }

        case p @ Pattern.Err(msg, p1) =>
          matchStep(p1, stream).map {
            _.map { pmatch =>
              Pattern.Match(p, Shifted(AST.Unexpected(msg, pmatch.toStream)))
            }
          }

        case p @ Except(p1, p2) =>
          matchStep(p1, stream) match {
            case Some(_) => None
            case None    => matchStep(p2, stream)
          }
      }
    }
    matchStep(pattern, stream)
  }

  //trait Pattern3
  //object Pattern3 {
  //  type P = Pattern3
  //
  //  trait Class
  //  object Class {
  //    final case object Normal  extends Class
  //    final case object Pattern extends Class
  //  }
  //
  //  // format: off
  //  /** Boundary Patterns */
  //  final case class Begin   ()                               extends P
  //  final case class End     ()                               extends P
  //
  //  /** Structural Patterns */
  //  final case class Nothing ()                               extends P
  //  final case class Seq     (pat1 : P     , pat2   : P)      extends P
  //  final case class Or      (pat1 : P     , pat2   : P)      extends P
  //  final case class Many    (pat  : P)                       extends P
  //  final case class Except  (not  : P     , pat    : P)      extends P
  //
  //  /** Meta Patterns */
  //  final case class Build   (pat  : P)                       extends P
  //  final case class Err     (msg  : String, pat    : P)      extends P
  //  final case class Tag     (tag  : String, pat    : P)      extends P
  //  final case class Cls     (cls  : Class , pat    : P)      extends P
  //
  //  /** Token Patterns */
  //  final case class Tok     (ast  : AST   , spaced : Spaced) extends P
  //  final case class Var     ()                               extends P
  //  final case class Cons    ()                               extends P
  //  final case class Opr     (maxPrec: Option[Int])           extends P
  //  final case class Num     ()                               extends P
  //  final case class Str     ()                               extends P
  //  final case class Block   ()                               extends P
  //  // format: on
  //}
  //
  //trait Match3 { val pat: Pattern3 }
  //object Match3 {
  //  type M = Match3
  //  type P = Pattern3
  //  val P = Pattern3
  //
  //  // format: off
  //  /** Boundary Matches */
  //  final case class Begin   (pat : P.Begin)                          extends M
  //  final case class End     (pat : P.End)                            extends M
  //
  //  /** Structural Matches */
  //  final case class Nothing (pat : P.Nothing)                        extends M
  //  final case class Seq     (pat : P.Seq      , elems : (M, M))      extends M
  //  final case class Or      (pat : P.Or       , elem  : Either[M,M]) extends M
  //  final case class Many    (pat : P.Many     , elems : List[M])     extends M
  //  final case class Except  (pat : P.Except   , elem  : M)           extends M
  //
  //  /** Meta Matches */
  //  final case class Build   (pat : P.Build    , ast   : SAST)        extends M
  //  final case class Err     (pat : P.Err      , ast   : SAST)        extends M
  //  final case class Tag     (pat : P.Tag      , elem  : M)           extends M
  //  final case class Cls     (pat : P.Err      , elem  : M)           extends M
  //
  //  /** Token Matches */
  //  final case class Tok     (pat : P.Tok      , ast   : SAST)        extends M
  //  final case class Var     (pat : P.Var      , ast   : SAST)        extends M
  //  final case class Cons    (pat : P.Cons     , ast   : SAST)        extends M
  //  final case class Opr     (pat : P.Opr      , ast   : SAST)        extends M
  //  final case class Num     (pat : P.Num      , ast   : SAST)        extends M
  //  final case class Str     (pat : P.Str      , ast   : SAST)        extends M
  //  final case class Block   (pat : P.Block    , ast   : SAST)        extends M
  //  // format: on
  //}

  implicit class OptionWhen(v: Option.type) {
    def when[A](cond: Boolean)(a: => A): Option[A] = if (cond) Some(a) else None
  }

  def matchOpt2(
    pattern: Pattern3,
    stream: Stream,
    lineBegin: Boolean,
    reversed: Boolean
  ): Option[MatchResult3] = {

    val P = Pattern3
    val M = Match3

    def matchList(p: Pattern3, stream: Stream): (List[Match3], Stream) = {
      @tailrec
      def go(stream: Stream, revOut: List[Match3]): (List[Match3], Stream) =
        step(p, stream) match {
          case None    => (revOut.reverse, stream)
          case Some(t) => go(t.stream, t.elem :: revOut)
        }
      go(stream, Nil)
    }

    def out(m: Match3, s: Stream)               = MatchResult3(m, s)
    def ret(m: Match3, s: Stream)               = Some(MatchResult3(m, s))
    def ret_(m: Match3)                         = Some(MatchResult3(m, stream))
    def retIf(b: Boolean)(m: Match3, s: Stream) = Option.when(b)(out(m, s))
    def retIf_(b: Boolean)(m: Match3)           = retIf(b)(m, stream)

    def stepWith(p: Pattern3, stream: Stream)(
      f: Match3 => Match3
    ): Option[MatchResult3] = step(p, stream).map(_.map(f))

    def matchByCls_[T](
      spaced: Pattern3.Spaced,
      f: Shifted[T] => Match3
    )(implicit tag: ClassTag[T]) = matchByCls[T](spaced)(a => Some(f(a)))

    def matchByCls[T](spaced: Pattern3.Spaced)(
      f: Shifted[T] => Option[Match3]
    )(implicit tag: ClassTag[T]): Option[MatchResult3] = stream match {
      case Shifted(off, tag(t)) :: ss =>
        val ok = spaced match {
          case None    => true
          case Some(s) => (s == (off > 0)) && (!t.isInstanceOf[AST.Block])
        }
        if (ok) f(Shifted(off, t)).map(out(_, ss)) else None
      case _ => None
    }

    def step(p: Pattern3, stream: Stream): Option[MatchResult3] = {

      p match {

        //// Boundary Matches ////

        case p @ P.Begin() => retIf_(lineBegin)(M.Begin(p))
        case p @ P.End()   => retIf_(stream.isEmpty)(M.End(p))

        //// Structural Matches ////

        case p @ P.Nothing() => ret_(M.Nothing(p))
        case p @ P.Seq(p1, p2) =>
          for {
            r1 <- step(p1, stream)
            r2 <- step(p2, r1.stream)
          } yield out(M.Seq(p, (r1.elem, r2.elem)), r2.stream)

        case p @ P.Or(p1, p2) =>
          val m1 = stepWith(p1, stream)(r => M.Or(p, Left(r)))
          m1.orElse(stepWith(p2, stream)(r => M.Or(p, Right(r))))

        case p @ P.Many(p1) =>
          val (lst, rest) = matchList(p1, stream)
          ret(M.Many(p, lst), rest)

        case p @ P.Except(p1, p2) =>
          step(p1, stream) match {
            case Some(_) => None
            case None    => stepWith(p2, stream)(M.Except(p, _))
          }

        //// Meta Matches ////

        // When performing reverse pattern match, tokens use right-offsets
        // instead of left ones, so we need to push them back before computing
        // AST.
        case p @ P.Build(pat2) =>
          step(pat2, stream).map {
            _.map { patMatch =>
              val stream = patMatch.toStream
              val ast =
                if (!reversed) buildASTFrom(stream).get
                else {
                  val (shiftedStream, off) = streamShift(0, stream.reverse)
                  val shiftedAst           = buildASTFrom(shiftedStream).get
                  shiftedAst.copy(off = off)
                }
              M.Build(p, ast)
            }
          }

        case p @ P.Err(msg, p1) =>
          step(p1, stream).map {
            _.map(m => M.Err(p, Shifted(AST.Unexpected(msg, m.toStream))))
          }

        case p @ P.Tag(tag, p1) => stepWith(p1, stream)(M.Tag(p, _))
        case p @ P.Cls(tag, p1) => stepWith(p1, stream)(M.Cls(p, _))

        //// Token Matches ////

        case p @ P.Tok(spaced, tok) =>
          stream match {
            case Shifted(off, t) :: ss =>
              val ok = spaced.forall(_ == (off > 0))
              Option.when(tok == t && ok)(out(M.Tok(p, Shifted(off, t)), ss))
            case _ => None
          }

        case p @ P.Var(spaced)   => matchByCls_(spaced, M.Var(p, _))
        case p @ P.Cons(spaced)  => matchByCls_(spaced, M.Cons(p, _))
        case p @ P.Num(spaced)   => matchByCls_(spaced, M.Num(p, _))
        case p @ P.Text(spaced)  => matchByCls_(spaced, M.Text(p, _))
        case p @ P.Block(spaced) => matchByCls_(spaced, M.Block(p, _))
        case p @ P.Opr(spaced, maxPrec) =>
          matchByCls[AST.Opr](spaced) { sast =>
            Option.when(maxPrec.forall(_ >= sast.el.prec))(M.Opr(p, sast))
          }
      }
    }
    step(pattern, stream)
  }

}
