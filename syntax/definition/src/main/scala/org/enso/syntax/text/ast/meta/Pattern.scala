package org.enso.syntax.text.ast.meta

import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.SAST
import org.enso.syntax.text.AST.Stream
import org.enso.syntax.text.prec.Operator

import scala.annotation.tailrec
import scala.reflect.ClassTag
import org.enso.data.Shifted
import org.enso.syntax.text.ast.Repr

////////////////////////////////////////////////////////////////////////////////
//// Pattern ///////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

object Pattern {
  type P      = Pattern
  type Spaced = Option[Boolean]

  // TODO: Refactorme
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

  trait Class
  object Class {
    final case object Normal  extends Class
    final case object Pattern extends Class
  }

  //// Primitive Constructors ////

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

  //// Smart Constructors ////

  object Tok {
    def apply(ast: AST): Tok = Tok(None, ast)
  }
  object Var {
    def apply():                Var = Var(None)
    def apply(spaced: Boolean): Var = Var(Some(spaced))
  }
  object Cons {
    def apply():                Cons = Cons(None)
    def apply(spaced: Boolean): Cons = Cons(Some(spaced))
  }
  object Opr {
    def apply():                Opr = Opr(None, None)
    def apply(spaced: Spaced):  Opr = Opr(spaced, None)
    def apply(spaced: Boolean): Opr = Opr(Some(spaced))
  }
  object Num {
    def apply():                Num = Num(None)
    def apply(spaced: Boolean): Num = Num(Some(spaced))
  }
  object Text {
    def apply():                Text = Text(None)
    def apply(spaced: Boolean): Text = Text(Some(spaced))
  }
  object Block {
    def apply():                Block = Block(None)
    def apply(spaced: Boolean): Block = Block(Some(spaced))
  }

  def Any(spaced: Spaced = None): Pattern =
    Var(spaced) |
    Cons(spaced) |
    Opr(spaced) |
    Num(spaced) |
    Text(spaced) |
    Block(spaced)
  def Any(spaced: Boolean): Pattern = Any(Some(spaced))
  def ErrTillEnd(msg: String)   = Any().tillEnd.err(msg)
  def ErrUnmatched(msg: String) = End() | ErrTillEnd(msg)
  def Expr()                    = Any().many1.build
//  def NonSpacedExpr_            = Any().but(Block()) ::
  def NonSpacedExpr()  = Any(spaced                        = false).many1.build
  def NonSpacedExpr_() = (Any().but(Block()) :: Any(spaced = false).many).build
  def SepList(pat: Pattern, div: Pattern): Pattern = pat :: (div :: pat).many
  def SepList(pat: Pattern, div: Pattern, err: String): Pattern = {
    val seg = pat | Any().till(div).err(err)
    SepList(seg, div)
  }

  def OprExpr(opr: String) = {
    val base = Except(Opr(None, Some(AST.Opr(opr).prec)), Any())
    base.many1.build
  }

  //// Utils ////

  def buildASTFrom(stream: Stream): Option[Shifted[AST]] =
    Operator.rebuild(stream)

  //// Conversions ////

  implicit def fromAST(ast: AST): Pattern = Tok(ast)

  //////////////////////////////////////////////////////////////////////////////
  //// Pattern.Match ///////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  object Match {
    type M = Match
    type P = Pattern
    val P = Pattern
    val A = AST

    //// Primitive Constructors ////

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

    //// Smart Constructors ////

    object Nothing {
      def apply(): Match.Nothing = Match.Nothing(Pattern.Nothing())
    }

    //// Result ////

    case class Result(elem: Match, stream: Stream) {
      def map(fn: Match => Match): Result = copy(elem = fn(elem))
    }

    implicit def reprForMatch: Repr.Of[Match] = {
      case _: Match.Begin => Repr.R
      //      case _: Match.End     => List()
      //      case _: Match.Nothing => List()
      //      case m: Match.Seq     => m.elems._1.toStream ++ m.elems._2.toStream
      //      case m: Match.Or      => m.elem.merge.toStream
      //      case m: Match.Many    => m.elems.flatMap(_.toStream)
      //      case m: Match.Except  => m.elem.toStream
      //      case m: Match.Build   => List(m.ast)
      //      case m: Match.Err     => List(m.ast)
      //      case m: Match.Tag     => m.elem.toStream
      //      case m: Match.Cls     => m.elem.toStream
      //      case m: Match.Tok     => List(m.ast)
      //      case m: Match.Var     => List(m.ast)
      //      case m: Match.Cons    => List(m.ast)
      //      case m: Match.Opr     => List(m.ast)
      //      case m: Match.Num     => List(m.ast)
      //      case m: Match.Text    => List(m.ast)
      //      case m: Match.Block   => List(m.ast)
    }
  }

  sealed trait Match {
    val pat: Pattern

    def toStream: Stream = this match {
      case _: Match.Begin   => List()
      case _: Match.End     => List()
      case _: Match.Nothing => List()
      case m: Match.Seq     => m.elems._1.toStream ++ m.elems._2.toStream
      case m: Match.Or      => m.elem.merge.toStream
      case m: Match.Many    => m.elems.flatMap(_.toStream)
      case m: Match.Except  => m.elem.toStream
      case m: Match.Build   => List(m.ast)
      case m: Match.Err     => List(m.ast)
      case m: Match.Tag     => m.elem.toStream
      case m: Match.Cls     => m.elem.toStream
      case m: Match.Tok     => List(m.ast)
      case m: Match.Var     => List(m.ast)
      case m: Match.Cons    => List(m.ast)
      case m: Match.Opr     => List(m.ast)
      case m: Match.Num     => List(m.ast)
      case m: Match.Text    => List(m.ast)
      case m: Match.Block   => List(m.ast)
    }

    def isValid: Boolean = ???
    //  def map(f: SAST => SAST): Match = ???
    def map(f: AST => AST): Match = ???
  }

}

////////////////////////////////////////////////////////////////////////////////
//// API ///////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

sealed trait Pattern {
  import Pattern._

  implicit class OptionWhen(v: Option.type) {
    def when[A](cond: Boolean)(a: => A): Option[A] = if (cond) Some(a) else None
  }

  ////////////////////////////
  //// Smart Constructors ////
  ////////////////////////////

  def ::(that: Pattern): Pattern = Seq(that, this)
  def !(that: Pattern):  Pattern = Except(that, this)
  def |(that: Pattern):  Pattern = Or(this, that)
  def |(msg: String):    Pattern = this | Err(msg, Nothing())
  def |?(tag: String):   Pattern = Tag(tag, this)

  def or(that: Pattern):  Pattern = Or(this, that)
  def or(msg: String):    Pattern = this | Err(msg, Nothing())
  def err(msg: String):   Pattern = Err(msg, this)
  def but(pat: Pattern):  Pattern = Except(pat, this)
  def many:               Pattern = Many(this)
  def many1:              Pattern = this :: this.many
  def tag(tag: String):   Pattern = Tag(tag, this)
  def opt:                Pattern = this | Nothing()
  def build:              Pattern = Build(this)
  def till(end: Pattern): Pattern = this.but(end).many
  def tillEnd:            Pattern = this :: End() // fixme: rename
  def fromBegin:          Pattern = Begin() :: this

  def matchRevUnsafe(stream: Stream, lineBegin: Boolean = false): Match.Result =
    this.matchUnsafe(stream, lineBegin = lineBegin, reversed = true)

  //////////////////////////////////
  //// Pattern Match Resolution ////
  //////////////////////////////////

  /** Unsafe variant of AST Macro tokens pattern matching. If you want to use
    * patterns that could not match all input tokens, use [[matchOpt]] instead.
    */
  def matchUnsafe(
    stream: Stream,
    lineBegin: Boolean = false,
    reversed: Boolean  = false
  ): Match.Result = {
    matchOpt(stream, lineBegin, reversed).getOrElse {
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
    stream0: Stream,
    lineBegin: Boolean,
    reversed: Boolean
  ): Option[Match.Result] = {

    val P = Pattern
    val M = Match

    def matchList(p: Pattern, stream: Stream): (List[Match], Stream) = {
      @tailrec
      def go(stream: Stream, revOut: List[Match]): (List[Match], Stream) =
        step(p, stream) match {
          case None    => (revOut.reverse, stream)
          case Some(t) => go(t.stream, t.elem :: revOut)
        }
      go(stream, Nil)
    }

    def stepWith(p: Pattern, stream: Stream)(
      f: Match => Match
    ): Option[Match.Result] = step(p, stream).map(_.map(f))

    def step(p: Pattern, stream: Stream): Option[Match.Result] = {

      def out(m: Match, s: Stream)               = Match.Result(m, s)
      def ret(m: Match, s: Stream)               = Some(Match.Result(m, s))
      def ret_(m: Match)                         = Some(Match.Result(m, stream))
      def retIf(b: Boolean)(m: Match, s: Stream) = Option.when(b)(out(m, s))
      def retIf_(b: Boolean)(m: Match)           = retIf(b)(m, stream)

      def matchByCls_[T](
        spaced: Pattern.Spaced,
        f: Shifted[T] => Match
      )(implicit tag: ClassTag[T]) = matchByCls[T](spaced)(a => Some(f(a)))

      def matchByCls[T](spaced: Pattern.Spaced)(
        f: Shifted[T] => Option[Match]
      )(implicit tag: ClassTag[T]): Option[Match.Result] = stream match {
        case Shifted(off, tag(t)) :: ss =>
          val ok = spaced match {
            case None    => true
            case Some(s) => (s == (off > 0)) && (!t.isInstanceOf[AST.Block])
          }
          if (ok) f(Shifted(off, t)).map(out(_, ss)) else None
        case _ => None
      }

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
        case p @ P.Build(p1) =>
          stepWith(p1, stream) { patMatch =>
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
    step(this, stream0)
  }
}
