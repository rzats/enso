package org.enso.syntax.text.test4

import monocle.macros.GenLens
import org.enso.data.List1._
import org.enso.data.List1
import org.enso.data.Shifted
import org.enso.data.Tree
import org.enso.syntax.text.ast.Repr.R
import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.opr
import org.enso.syntax.text.ast.text
import java.util.UUID

import scala.annotation.tailrec

import cats.Functor
import cats.implicits._
import cats.derived._

object AST {

  def map[F[_]: Functor, A, B](t: F[A])(f: A => B): F[B] = Functor[F].map(t)(f)

  //////////////////////////////////////////////////////////////////////////////
  //// Reexports ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Assoc = opr.Assoc

  val Assoc = opr.Assoc
  val Prec  = opr.Prec

  //////////////////////////////////////////////////////////////////////////////
  //// Definition //////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //// Structure ////

  type AST      = ASTOf[FixedAST]
  type Shape    = ShapeOf[FixedAST]
  type ASTOf[T] = Tagged[ShapeOf[T]]
  type FixedAST = Fix[ASTOf]

  //// Aliases ////

  type SAST        = Shifted[AST]
  type _Stream[T]  = List[Shifted[T]]
  type _Stream1[T] = List1[Shifted[T]]
  type Stream      = _Stream[Shape]
  type Stream1     = _Stream1[Shape]
  type ID          = UUID

  //// API ////

  implicit class ASTOps[T[_]](ast: T[FixedAST]) {
    import AST.Scheme.implicits._

    def repr:     Repr = ast.repr
    def span:     Int  = repr.span
    def byteSpan: Int  = repr.byteSpan

    def map(f: AST => AST)(
      implicit
      functorForT: Functor[T],
      reprForT: Repr.Of[T[FixedAST]]
    ): T[FixedAST] = Functor[T].map(ast)(_.map(f))

    def mapWithOff(f: (Int, AST) => AST)(
      implicit
      functorForT: Functor[T],
      offsetZipForT: OffsetZip[T, FixedAST],
      reprForT: Repr.Of[T[FixedAST]]
    ): T[FixedAST] = {
      val zipped = OffsetZip(ast)
      Functor[T].map(zipped) { case (i, fx) => fx.map(f(i, _)) }
    }

    def traverseWithOff(f: (Int, AST) => AST)(
      implicit
      functorForT: Functor[T],
      offsetZipForT: OffsetZip[T, FixedAST],
      reprForT: Repr.Of[T[FixedAST]]
    ): T[FixedAST] = {
      def go(i: Int, t: AST): AST = {
        val newStruct = t.struct.mapWithOff { (j, ast) =>
          val off = i + j
          go(off, f(off, ast))
        }
        t.copy(struct = newStruct)
      }
      mapWithOff { (off, ast) =>
        go(off, f(off, ast))
      }
    }
  }

  def tokenize(ast: Shape): Shifted.List1[Shape] = {
    @tailrec
    def go(ast: Shape, out: AST.Stream): Shifted.List1[Shape] =
      ast match {
        case t: App.Prefix => go(t.fn, Shifted(t.off, t.arg.struct) :: out)
        case anyAst        => Shifted.List1(anyAst, out)
      }
    go(ast, List())
  }

  object implicits extends Ident.implicits with Literal.implicits {
    implicit def stringToAST(str: String): Shape = {
      if (str == "") throw new Error("Empty literal")
      if (str == "_") Blank()
      else if (str.head.isLower) Var(str)
      else if (str.head.isUpper) Cons(str)
      else Opr(str)
    }
  }

  //// Instances ////

  implicit def offsetZipForStream[T: Repr.Of]: OffsetZip[_Stream, T] =
    stream => {
      var off = 0
      stream.map { t =>
        off += t.off
        val out = t.map((off, _))
        off += Repr.span(t.el)
        out
      }
    }

  //////////////////////////////////////////////////////////////////////////////
  //// OffsetZip ///////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  trait OffsetZip[F[A], A] {
    def zipWithOffset(t: F[A]): F[(Int, A)]
  }
  object OffsetZip {
    def apply[F[A], A](implicit ev: OffsetZip[F, A]): OffsetZip[F, A] = ev
    def apply[F[A], A](t: F[A])(implicit ev: OffsetZip[F, A]): F[(Int, A)] =
      OffsetZip[F, A].zipWithOffset(t)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Catamorphism ////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  case class Fix[F[_]](unFix: Fix.Body[F]) {
    def map(f: Fix.Body[F] => Fix.Body[F]): Fix[F] =
      copy(unFix = f(unFix))
  }
  object Fix {
    type Body[F[_]] = F[Fix[F]]
    implicit def reprForFix[F[_]](
      implicit ev: Repr.Of[Fix.Body[F]]
    ): Repr.Of[Fix[F]] = t => Repr.of(t.unFix)
  }

  implicit def fix(t: Fix.Body[ASTOf]): Fix[ASTOf] = Fix(t)
  implicit def fix2[T[_] <: ShapeOf[_]](
    t: T[FixedAST]
  )(implicit ev: Repr.Of[T[FixedAST]]): Fix[ASTOf] = {
    val t1 = Tagged(t)
    val t2 = t1.asInstanceOf[Fix.Body[ASTOf]] // FIXME
    Fix(t2)
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Tagged //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** [[Tagged]] is a AST nodes wrapper which adds [[ID]] information and
    * caches the repr of the node.
    *
    * @param struct Is the structure of the AST node. In most cases, it is a
    *               subtype of [[Shape]].
    * @param id     Is the unique AST Node ID assigned by parser from the marker
    *               map.
    */
  case class Tagged[+F: Repr.Of](struct: F, id: Option[ID] = None) {
    val repr: Repr = Repr.of(struct)
  }

  implicit class TaggedOps[T: Repr.Of](t: Tagged[T]) {
    def withNewID() = t.copy(id = Some(UUID.randomUUID()))
  }

  //// Conversions ////

  implicit def toTagged[T: Repr.Of](t: T):  Tagged[T]          = Tagged(t, None)
  implicit def fromTagged[T](t: Tagged[T]): T                  = t.struct
  implicit def reprForWithData:             Repr.Of[Tagged[_]] = _.repr
  implicit def taggedToASTOps[T[_]](v: Tagged[T[FixedAST]]): ASTOps[T] =
    v.struct

  //////////////////////////////////////////////////////////////////////////////
  //// Scheme //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** [[Shape]] defines the shape of an AST node. It contains information about
    * the layout of elements and spacing between them.
    * @tparam T The type of elements.
    */
  sealed trait ShapeOf[T]

//  type Shape = _Shape[FixedAST]
  implicit def functorForScheme: Functor[ShapeOf] = semi.functor

  object Scheme {
    object implicits extends implicits
    trait implicits {
      import Ident.implicits._
//      import App.implicits._

      // TODO: Should be auto-generated with Shapeless
      implicit def reprForScheme: Repr.Of[ShapeOf[FixedAST]] = {
        case t: Blank => Ident.implicits.reprForBlank.of(t)
        case t: Var   => Ident.implicits.reprForVar.of(t)
        case t: Cons  => Ident.implicits.reprForCons.of(t)
//        case t: App._Prefix[_AST] =>
//          App.implicits.reprForPrefix[_AST].of(t)
      }

      // TODO: Should be auto-generated with Shapeless
      implicit def offsetZipForScheme[T: Repr.Of]: OffsetZip[ShapeOf, T] = {
        case t: Ident._Var[T]  => OffsetZip(t)
        case t: Ident._Cons[T] => OffsetZip(t)
//        case t: App._Prefix[T] => OffsetZip(t)
      }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Phantom /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  /** Phantom type. Use with care, as Scala cannot prove its proper usage. When
    * a type is phantom, then its last type argument is not used and we can
    * safely coerce it to something else.
    */
  trait Phantom
  implicit class PhantomOps[T[_] <: Phantom](ident: T[_]) {
    def coerce[S]: T[S] = ident.asInstanceOf[T[S]]
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Invalid /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Invalid      = _Invalid[FixedAST]
  type Unrecognized = _Unrecognized[FixedAST]
  type Unexpected   = _Unexpected[FixedAST]

  sealed trait _Invalid[T]                                   extends ShapeOf[T]
  case class _Unrecognized[T](str: String)                   extends _Invalid[T] with Phantom
  case class _Unexpected[T](msg: String, stream: _Stream[T]) extends _Invalid[T]

  implicit def functorForInvalid:      Functor[_Invalid]         = semi.functor
  implicit def functorForUnexpected:   Functor[_Unexpected]      = semi.functor
  implicit def functorForUnrecognized: Functor[_Unrecognized]    = semi.functor
  implicit def reprForUnrecognized:    Repr.Of[_Unrecognized[_]] = _.str
  implicit def reprForUnexpected[T: Repr.Of]: Repr.Of[_Unexpected[T]] =
    t => Repr.of(t.stream)
  implicit def offsetZipForUnrecognized[T]: OffsetZip[_Unrecognized, T] =
    t => t.coerce
  implicit def offsetZipForUnexpected[T: Repr.Of]: OffsetZip[_Unexpected, T] =
    t => t.copy(stream = OffsetZip(t.stream))

  //////////////////////////////////////////////////////////////////////////////
  //// Ident ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Ident = _Ident[FixedAST]
  sealed trait _Ident[T] extends ShapeOf[T] with Phantom { val name: String }

  implicit class IdentOps(t: Tagged[Ident]) {
    def name: String = t.struct.name
  }

  type Blank = Ident.Blank
  type Var   = Ident.Var
  type Cons  = Ident.Cons
  type Opr   = Ident.Opr
  type Mod   = Ident.Mod

  val Blank = Ident.Blank
  val Var   = Ident.Var
  val Cons  = Ident.Cons
  val Opr   = Ident.Opr
  val Mod   = Ident.Mod

  object Ident {
    case class InvalidSuffix[T](elem: Tagged[Ident], suffix: String)
        extends _Invalid[T]

    //// Definition ////

    type Blank = _Blank[FixedAST]
    type Var   = _Var[FixedAST]
    type Cons  = _Cons[FixedAST]
    type Opr   = _Opr[FixedAST]
    type Mod   = _Mod[FixedAST]

    case class _Blank[T]()            extends _Ident[T] { val name = "_" }
    case class _Var[T](name: String)  extends _Ident[T]
    case class _Cons[T](name: String) extends _Ident[T]
    case class _Mod[T](name: String)  extends _Ident[T]
    case class _Opr[T](name: String) extends _Ident[T] {
      val (prec, assoc) = opr.Info.of(name)
    }

    //// Companions ////

    object Blank {
      def apply():           Blank   = _Blank()
      def unapply(t: Blank): Boolean = true
    }

    object Var {
      def apply(name: String): Var            = _Var(name)
      def unapply(t: Var):     Option[String] = Some(t.name)
      trait implicits {
        implicit def stringToVar(str: String): Var = Var(str)
      }
    }

    object Cons {
      def apply(name: String): Cons           = _Cons(name)
      def unapply(t: Cons):    Option[String] = Some(t.name)
      trait implicits {
        implicit def stringToCons(str: String): Cons = Cons(str)
      }
    }

    object Opr {
      def apply(name: String): Opr            = _Opr(name)
      def unapply(t: Opr):     Option[String] = Some(t.name)
      trait implicits {
        implicit def stringToOpr(str: String): Opr = Opr(str)
      }
      val app: Opr = Opr(" ")
    }

    object Mod {
      def apply(name: String): Mod            = _Mod(name)
      def unapply(t: Mod):     Option[String] = Some(t.name)
      trait implicits {
        implicit def stringToMod(str: String): Mod = Mod(str)
      }
    }

    //// Instances ////

    object implicits extends implicits
    trait implicits
        extends Scheme.implicits
        with Var.implicits
        with Cons.implicits
        with Opr.implicits
        with Mod.implicits {
      implicit def reprForBlank:         Repr.Of[_Blank[_]]   = _.name
      implicit def reprForVar:           Repr.Of[_Var[_]]     = _.name
      implicit def reprForCons:          Repr.Of[_Cons[_]]    = _.name
      implicit def reprForOpr:           Repr.Of[_Opr[_]]     = _.name
      implicit def reprForMod:           Repr.Of[_Mod[_]]     = _.name
      implicit def functorForIdent:      Functor[_Ident]      = semi.functor
      implicit def functorForBlank:      Functor[_Blank]      = semi.functor
      implicit def functorForVar:        Functor[_Var]        = semi.functor
      implicit def functorForOpr:        Functor[_Opr]        = semi.functor
      implicit def functorForMod:        Functor[_Mod]        = semi.functor
      implicit def offsetZipForBlank[T]: OffsetZip[_Blank, T] = t => t.coerce
      implicit def offsetZipForVar[T]:   OffsetZip[_Var, T]   = t => t.coerce
      implicit def offsetZipForCons[T]:  OffsetZip[_Cons, T]  = t => t.coerce
      implicit def offsetZipForOpr[T]:   OffsetZip[_Opr, T]   = t => t.coerce
      implicit def offsetZipForMod[T]:   OffsetZip[_Mod, T]   = t => t.coerce

      implicit def stringToIdent(str: String): Ident = {
        if (str == "") throw new Error("Empty literal")
        if (str == "_") Blank()
        else if (str.head.isLower) Var(str)
        else if (str.head.isUpper) Cons(str)
        else Opr(str)
      }
    }
  }
  import Ident.implicits._

  //////////////////////////////////////////////////////////////////////////////
  //// Literal /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Literal = _Literal[FixedAST]
  sealed trait _Literal[T] extends ShapeOf[T]

//  implicit def functorForLiteral:    Functor[_Literal]    = semi.functor

  object Literal {

    object implicits extends implicits
    trait implicits  extends Number.implicits

    ////////////////
    //// Number ////
    ////////////////

    type Number = _Number[FixedAST]
    case class _Number[T](base: Option[String], int: String)
        extends _Literal[T]
        with Phantom

    object Number {
      def apply(i: String):            Number = _Number(None, i)
      def apply(b: String, i: String): Number = _Number(Some(b), i)
      def apply(i: Int):               Number = Number(i.toString)
      def apply(b: Int, i: String):    Number = Number(b.toString, i)
      def apply(b: String, i: Int):    Number = Number(b, i.toString)
      def apply(b: Int, i: Int):       Number = Number(b.toString, i.toString)

      object implicits extends implicits
      trait implicits {
        implicit def reprForNumber: Repr.Of[_Number[_]] =
          t => t.base.map(_ + "_").getOrElse("") + t.int
        implicit def functorForNumber: Functor[_Number] = semi.functor
        implicit def offsetZipForNumber[T]: OffsetZip[_Number, T] =
          t => t.coerce
      }

      //// DanglingBase ////

//      case class DanglingBase(base: String) extends AST.Invalid {
//        val repr               = base + '_'
//        def map(f: AST => AST) = this
//      }
    }

    //////////////
    //// Text ////
    //////////////

    type Text = _Text[FixedAST]
    sealed trait _Text[T] extends ShapeOf[T]
    object Text {

      //// Definition ////

      type Line[T]  = List[T]
      type Block[T] = List1[Line[T]]

      type Raw          = _Raw[FixedAST]
      type Interpolated = _Interpolated[FixedAST]
      type Unclosed     = _Unclosed[FixedAST]

      case class _Raw[T](quote: Quote, lines: Raw.Block[T]) extends _Text[T] {
        val quoteChar: Char = '"'
      }
      case class _Interpolated[T](quote: Quote, lines: Interpolated.Block[T])
          extends _Text[T] {
        val quoteChar: Char = '\''
      }
      case class _Unclosed[T](text: _Text[T]) extends AST._Invalid[T]

      object Raw {
        type Segment[T] = Text.Segment._Raw[T]
        type Line[T]    = Text.Line[Segment[T]]
        type Block[T]   = Text.Block[Segment[T]]
      }

      object Interpolated {
        type Segment[T] = Text.Segment._Interpolated[T]
        type Line[T]    = Text.Line[Segment[T]]
        type Block[T]   = Text.Block[Segment[T]]
      }

      // Instances ////

      object implicits extends implicits
      trait implicits extends Segment.implicits {
        implicit def reprForTextRaw[T]:      Repr.Of[_Raw[T]]          = _ => ???
        implicit def reprForTextInt[T]:      Repr.Of[_Interpolated[T]] = _ => ???
        implicit def reprForTextUnclosed[T]: Repr.Of[_Unclosed[T]]     = _ => ???

        implicit def functorForTextRaw[T]: Functor[_Raw] =
          semi.functor
        implicit def functorForTextInterpolated[T]: Functor[_Interpolated] =
          semi.functor
        implicit def functorForTextUnclosed[T]: Functor[_Unclosed] =
          semi.functor
        implicit def offsetZipForTextRaw[T]: OffsetZip[_Raw, T] =
          t => t.copy(lines = t.lines.map(_.map(OffsetZip(_))))
        implicit def offsetZipForTextInt[T]: OffsetZip[_Interpolated, T] =
          t => t.copy(lines = t.lines.map(_.map(OffsetZip(_))))
        implicit def offsetZipForUnclosed[T]: OffsetZip[_Unclosed, T] =
          t => t.copy(text = OffsetZip(t.text))
        implicit def offsetZipForText[T]: OffsetZip[_Text, T] = {
          case t: _Raw[T]          => OffsetZip(t)
          case t: _Interpolated[T] => OffsetZip(t)
        }
      }

      ///////////////
      //// Quote ////
      ///////////////

      sealed trait Quote {
        val asInt: Int
      }
      object Quote {
        final case object Single extends Quote { val asInt = 1 }
        final case object Triple extends Quote { val asInt = 3 }
      }

      /////////////////
      //// Segment ////
      /////////////////

      sealed trait Segment[T]
      object Segment {

        //// Definition ////

        type Interpolated = _Interpolated[FixedAST]
        type Raw          = _Raw[FixedAST]
        sealed trait _Interpolated[T] extends Segment[T]
        sealed trait _Raw[T]          extends _Interpolated[T]

        type Plain = _Plain[FixedAST]
        type Expr  = _Expr[FixedAST]
        case class _Plain[T](value: String)   extends _Raw[T] with Phantom
        case class _Expr[T](value: Option[T]) extends _Interpolated[T]

        //// Instances ////

        object implicits extends implicits
        trait implicits {
          implicit def reprForTextSegPlain[T]: Repr.Of[_Plain[T]] =
            _.value
          implicit def reprForTextSegExpr[T: Repr.Of]: Repr.Of[_Expr[T]] =
            R + '`' + _.value + '`'
          implicit def functorForTextSegPlain[T]: Functor[_Plain] =
            semi.functor
          implicit def functorForTextSegExpr[T]: Functor[_Expr] =
            semi.functor
          implicit def offsetZipForTextSegExpr[T]: OffsetZip[_Expr, T] =
            _.map((0, _))
          implicit def offsetZipForTextSegPlain[T]: OffsetZip[_Plain, T] =
            t => t.coerce
          implicit def reprForTextSegRaw[T]: Repr.Of[_Raw[T]] = {
            case t: _Plain[T] => t.repr
          }
          implicit def reprForTextSegInt[T: Repr.Of]
            : Repr.Of[_Interpolated[T]] = {
            case t: _Plain[T] => t.repr
            case t: _Expr[T]  => t.repr
          }
          implicit def functorForTextSegRaw[T]: Functor[_Raw] =
            semi.functor
          implicit def functorForTextSegInt[T]: Functor[_Interpolated] =
            semi.functor
          implicit def offsetZipForTextSegRaw[T]: OffsetZip[_Raw, T] = {
            case t: _Plain[T] => OffsetZip(t)
          }
          implicit def offsetZipForTextSegInt[T]
            : OffsetZip[_Interpolated, T] = {
            case t: _Plain[T] => OffsetZip(t)
            case t: _Expr[T]  => OffsetZip(t)
          }

          implicit def fromString[T](str: String): _Plain[T] = _Plain(str)
        }

//        trait Escape extends Interpolated
//        val Escape = text.Escape

      }

    }

  }

  //////////////////////////////////////////////////////////////////////////////
  //// App /////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  //// Definition ////

  type App = _App[FixedAST]
  sealed trait _App[T] extends ShapeOf[T]
  object App {

    //// Constructors ////

    type Prefix = _Prefix[FixedAST]
    type Infix  = _Infix[FixedAST]
    case class _Prefix[T](_fn: T, off: Int, _arg: T) extends _App[T]
    case class _Infix[T](_larg: T, loff: Int, opr: Opr, roff: Int, _rarg: T)
        extends _App[T]

    object Prefix {
      def unapply(t: Prefix) = Some((t.fn, t.arg))
      def apply(fn: AST, off: Int = 1, arg: AST): Prefix =
        _Prefix(fn, off, arg)
      def apply(fn: AST, arg: AST): Prefix =
        Prefix(fn, 1, arg)
    }

    implicit def taggedPrefixOps(t: Tagged[Prefix]): PrefixOps = t.struct
    implicit class PrefixOps(t: Prefix) {
      def fn:            AST    = t._fn.unFix
      def arg:           AST    = t._arg.unFix
      def fn_=(v: AST):  Prefix = t.copy(_fn = fix(v))
      def arg_=(v: AST): Prefix = t.copy(_arg = fix(v))
    }

    object Infix {
      def unapply(t: Infix) = Some((t.larg, t.opr, t.rarg))
      def apply(
        larg: AST,
        loff: Int,
        opr: Opr,
        roff: Int,
        rarg: AST
      ): Infix = _Infix(larg, loff, opr, roff, rarg)

      def apply(
        larg: AST,
        loff: Int,
        opr: Opr,
        rarg: AST
      ): Infix = Infix(larg, loff, opr, 1, rarg)

      def apply(
        larg: AST,
        opr: Opr,
        roff: Int,
        rarg: AST
      ): Infix = Infix(larg, 1, opr, roff, rarg)

      def apply(larg: AST, opr: Opr, rarg: AST): Infix =
        Infix(larg, 1, opr, 1, rarg)
    }

    implicit def taggedInfixOps(t: Tagged[Infix]): InfixOps = t.struct
    implicit class InfixOps(t: Infix) {
      def larg:           AST   = t._larg.unFix
      def rarg:           AST   = t._rarg.unFix
      def opr:            AST   = t.opr
      def larg_=(v: AST): Infix = t.copy(_larg = fix(v))
      def rarg_=(v: AST): Infix = t.copy(_rarg = fix(v))
      def opr_=(v: Opr):  Infix = t.copy(opr = v)
    }

    //// Instances ////

    object implicits extends implicits
    trait implicits {
      implicit def reprForPrefix[T: Repr.Of]: Repr.Of[_Prefix[T]] =
        t => R + t._fn + t.off + t._arg
      implicit def reprForInfix[T: Repr.Of]: Repr.Of[_Infix[T]] =
        t => R + t._larg + t.loff + t.opr + t.roff + t._rarg
      implicit def functorForPrefix: Functor[_Prefix] = semi.functor
      implicit def functorForInfix:  Functor[_Infix]  = semi.functor
      implicit def offsetZipForPrefix[T: Repr.Of]: OffsetZip[_Prefix, T] =
        t => t.copy(_fn = (0, t._fn), _arg = (Repr.span(t._fn) + t.off, t._arg))
      implicit def offsetZipForInfix[T: Repr.Of]: OffsetZip[_Infix, T] =
        t => {
          val rargSpan = (R + t._larg + t.loff + t.opr + t.roff).span
          t.copy(_larg = (0, t._larg), _rarg = (rargSpan, t._rarg))
        }
    }

    /////////////////
    //// Section ////
    /////////////////

    type Section = _Section[FixedAST]
    sealed trait _Section[T] extends _App[T]
    object Section {

      //// Constructors ////

      type Left  = _Left[FixedAST]
      type Right = _Right[FixedAST]
      type Sides = _Sides[FixedAST]

      case class _Left[T](_arg: T, off: Int, opr: Opr)  extends _Section[T]
      case class _Right[T](opr: Opr, off: Int, _arg: T) extends _Section[T]
      case class _Sides[T](opr: Opr)                    extends _Section[T] with Phantom

      object Left {
        def unapply(t: Left) = Some((t.arg, t.opr))
        def apply(arg: AST, off: Int, opr: Opr): Left =
          _Left(arg, off, opr)
        def apply(arg: AST, opr: Opr): Left = Left(arg, 1, opr)
      }
      implicit def taggedLeftOps(t: Tagged[Left]): LeftOps = t.struct
      implicit class LeftOps(t: Left) {
        def arg           = t._arg.unFix
        def arg_=(v: AST) = t.copy(_arg = v)
      }

      object Right {
        def unapply(t: Right) = Some((t.opr, t.arg))
        def apply(opr: Opr, off: Int, arg: AST): Right =
          _Right(opr, off, arg)
        def apply(opr: Opr, arg: AST): Right = Right(opr, 1, arg)
      }
      implicit def taggedRightOps(t: Tagged[Right]): RightOps = t.struct
      implicit class RightOps(t: Right) {
        def arg           = t._arg.unFix
        def arg_=(v: AST) = t.copy(_arg = v)
      }

      object Sides {
        def unapply(t: Sides) = Some(t.opr)
        def apply(opr: Opr): Sides = _Sides(opr)
      }
      implicit def taggedSidesOps(t: Tagged[Sides]): SidesOps = t.struct
      implicit class SidesOps(t: Sides) {
        def opr           = t.struct.opr
        def opr_=(v: Opr) = t.struct.copy(opr = v)
      }

      //// Instances ////

      object implicits extends implicits
      trait implicits {
        implicit def reprForLeft[T: Repr.Of]: Repr.Of[_Left[T]] =
          t => R + t._arg + t.off + t.opr
        implicit def reprForRight[T: Repr.Of]: Repr.Of[_Right[T]] =
          t => R + t.opr + t.off + t._arg
        implicit def reprForSides[T: Repr.Of]: Repr.Of[_Sides[T]] =
          t => R + t.opr
        implicit def functorForLeft:  Functor[_Left]  = semi.functor
        implicit def functorForRight: Functor[_Right] = semi.functor
        implicit def functorForSides: Functor[_Sides] = semi.functor
        implicit def offsetZipForLeft[T]: OffsetZip[_Left, T] =
          t => t.copy(_arg = (0, t._arg))
        implicit def offsetZipForRight[T]: OffsetZip[_Right, T] =
          t => t.copy(_arg = (Repr.span(t.opr) + t.off, t._arg))
        implicit def offsetZipForSides[T]: OffsetZip[_Sides, T] =
          t => t.coerce
      }
    }
  }

  ///

  //

  //

  //////////////////////////////////////////////////////////////////////////////
  //// Block ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  val newline = R + '\n'

  type Block = _Block[FixedAST]
  case class _Block[T](
    typ: Block.Type,
    indent: Int,
    emptyLines: List[Int],
    firstLine: Block.Line[T],
    lines: List[Block.Line[Option[T]]]
  ) extends ShapeOf[T]

  object Block {
    sealed trait Type
    final case object Continuous    extends Type
    final case object Discontinuous extends Type

    def apply[T](
      typ: Type,
      indent: Int,
      emptyLines: List[Int],
      firstLine: Line[T],
      lines: List[Line[Option[T]]]
    ): _Block[T] =
      _Block(typ, indent, emptyLines, firstLine, lines)

    def apply[T](
      typ: Type,
      indent: Int,
      firstLine: Line[T],
      lines: List[Line[Option[T]]]
    ): _Block[T] =
      Block(typ, indent, List(), firstLine, lines)

    def unapply[T](
      t: _Block[T]
    ): Option[(Int, Line[T], List[Line[Option[T]]])] =
      Some((t.indent, t.firstLine, t.lines))

    //// Line ////

    case class Line[+T](elem: T, off: Int)
    object Line {
      type _Optional[T] = Line[Option[T]]
      type _Required[T] = Line[T]
      type Optional     = _Optional[FixedAST]
      type Required     = _Required[FixedAST]

      def apply[T](elem: T): Line[T] = Line(elem, 0)
    }

  }

  //////////////////////////////////////////////////////////////////////////////
  //// Module //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Module = _Module[FixedAST]
  case class _Module[T](lines: List1[Module._Line[T]]) extends ShapeOf[T]

  object Module {
    type _Line[T] = Block.Line._Optional[T]
    type Line     = _Line[FixedAST]
    def apply(ls: List1[Line]):         Module = _Module(ls)
    def apply(l: Line):                 Module = Module(List1(l))
    def apply(l: Line, ls: Line*):      Module = Module(List1(l, ls.to[List]))
    def apply(l: Line, ls: List[Line]): Module = Module(List1(l, ls))
  }

  def main() {

    import Ident.implicits._

    val foo    = Var("foo")
    val bar    = Var("foo")
    val plus   = Opr("+")
    val ttt2   = foo: Shape
    val fooAST = foo: Shape

    val foox = foo: AST

//    val foo    = Var("foo")
//    val foo2 = fix2(foo): FixedAST

//    println(foox.withNewID())
    val tfoo  = Var("foo")
    val tfoo2 = fix2(tfoo): FixedAST
    val tfoo3 = tfoo: FixedAST

    val l1 = Block.Line(tfoo3): Block.Line.Required

    println("..........")
    println(tfoo2)
    println(tfoo3)

    val x1   = toTagged(foo)
    var app1 = App.Prefix(fooAST, 0, bar)

    fooAST match {
      case t: App        => println("It was APP 1")
      case t: App.Prefix => println("It was APP 2")
      case t: Ident      => println("It was Ident")
      case t: Var        => println("It was VAR")
    }
  }

  //
  //  //////////////////////////////////////////////////////////////////////////////
  //  //// Macro ///////////////////////////////////////////////////////////////////
  //  //////////////////////////////////////////////////////////////////////////////
  //
  //  sealed trait Macro extends AST
  //  object Macro {
  //
  //    import org.enso.syntax.text.ast.meta.Pattern
  //
  //    //// Matched ////
  //
  //     case class Match(
  //      pfx: Option[Pattern.Match],
  //      segs: Shifted.List1[Match.Segment],
  //      resolved: AST,
  //      id: Option[ID] = None
  //    ) extends Macro {
  //      val repr = {
  //        val pfxStream = pfx.map(_.toStream.reverse).getOrElse(List())
  //        val pfxRepr   = pfxStream.map(t => R + t.el + t.off)
  //        val segsRepr  = segs.map(_.repr)
  //        R + pfxRepr + segsRepr
  //      }
  //      def setID(newID: ID)                 = copy(id = Some(newID))
  //      def map(f: AST => AST)               = this
  //      def mapWithOff(f: (Int, AST) => AST) = this
  //      def path(): List1[AST] = segs.toList1().map(_.el.head)
  //    }
  //    object Match {
  //       case class Segment(head: Ident, body: Pattern.Match) {
  //        val repr = R + head + body
  //        def toStream: AST.Stream = ??? // Shifted(head) :: body.toStream
  //        def isValid:  Boolean    = body.isValid
  //        def map(f: Pattern.Match => Pattern.Match): Segment =
  //          copy(body = f(body))
  //      }
  //      object Segment {
  //        def apply(head: Ident): Segment = Segment(head, Pattern.Match.Nothing())
  //      }
  //    }
  //
  //    //// Ambiguous ////
  //
  //     case class Ambiguous(
  //      segs: Shifted.List1[Ambiguous.Segment],
  //      paths: Tree[AST, Unit],
  //      id: Option[ID] = None
  //    ) extends Macro {
  //      val repr                             = R + segs.map(_.repr)
  //      def setID(newID: ID)                 = copy(id = Some(newID))
  //      def map(f: AST => AST)               = this
  //      def mapWithOff(f: (Int, AST) => AST) = this
  //    }
  //    object Ambiguous {
  //       case class Segment(head: AST, body: Option[SAST]) extends Symbol {
  //        val repr = R + head + body
  //      }
  //      object Segment {
  //        def apply(head: AST): Segment = Segment(head, None)
  //      }
  //    }
  //
  //    //// Definition ////
  //
  //    type Definition = __Definition__
  //     case class __Definition__(
  //      back: Option[Pattern],
  //      init: List[Definition.Segment],
  //      last: Definition.LastSegment,
  //      fin: Definition.Finalizer
  //    ) {
  //      def path: List1[AST] = init.map(_.head) +: List1(last.head)
  //      def fwdPats: List1[Pattern] =
  //        init.map(_.pattern) +: List1(last.pattern.getOrElse(Pattern.Nothing()))
  //    }
  //    object Definition {
  //      import Pattern._
  //      type Finalizer = (Option[Pattern.Match], List[Macro.Match.Segment]) => AST
  //
  //       case class Segment(head: AST, pattern: Pattern) {
  //        def map(f: Pattern => Pattern): Segment = copy(pattern = f(pattern))
  //      }
  //      object Segment {
  //        type Tup = (AST, Pattern)
  //        def apply(t: Tup): Segment = Segment(t._1, t._2)
  //      }
  //
  //       case class LastSegment(head: AST, pattern: Option[Pattern]) {
  //        def map(f: Pattern => Pattern): LastSegment =
  //          copy(pattern = pattern.map(f))
  //      }
  //      object LastSegment {
  //        type Tup = (AST, Option[Pattern])
  //        def apply(t: Tup): LastSegment = LastSegment(t._1, t._2)
  //      }
  //
  //      def apply(back: Option[Pattern], t1: Segment.Tup, ts: List[Segment.Tup])(
  //        fin: Finalizer
  //      ): Definition = {
  //        val segs    = List1(t1, ts)
  //        val init    = segs.init
  //        val lastTup = segs.last
  //        val last    = (lastTup._1, Some(lastTup._2))
  //        Definition(back, init, last, fin)
  //      }
  //
  //      def apply(back: Option[Pattern], t1: Segment.Tup, ts: Segment.Tup*)(
  //        fin: Finalizer
  //      ): Definition = Definition(back, t1, ts.toList)(fin)
  //
  //      def apply(t1: Segment.Tup, t2_ : Segment.Tup*)(
  //        fin: Finalizer
  //      ): Definition = Definition(None, t1, t2_.toList)(fin)
  //
  //      def apply(initTups: List[Segment.Tup], lastHead: AST)(
  //        fin: Finalizer
  //      ): Definition =
  //        Definition(None, initTups, (lastHead, None), fin)
  //
  //      def apply(t1: Segment.Tup, last: AST)(fin: Finalizer): Definition =
  //        Definition(List(t1), last)(fin)
  //      //
  //      //      def apply(backPat: Option[Pattern], t1: Segment.Tup, ts: Segment.Tup*)(
  //      //        fin: Finalizer
  //      //      ): Definition =
  //      //        Definition(backPat, List1(t1, ts: _*), fin)
  //
  //      def apply(
  //        back: Option[Pattern],
  //        initTups: List[Segment.Tup],
  //        lastTup: LastSegment.Tup,
  //        fin: Finalizer
  //      ): Definition = {
  //        type PP = Pattern => Pattern
  //        val checkValid: PP = _ | ErrTillEnd("unmatched pattern")
  //        val checkFull: PP  = TillEndMarkUnmatched(_, "unmatched tokens")
  //
  //        val addInitChecks: List[Segment] => List[Segment] =
  //          _.map(_.map(checkValid).map(checkFull))
  //
  //        val addLastCheck: LastSegment => LastSegment =
  //          _.map(checkValid)
  //
  //        val initSegs           = initTups.map(Segment(_))
  //        val lastSeg            = LastSegment(lastTup)
  //        val backPatWithCheck   = back.map(checkValid)
  //        val initSegsWithChecks = addInitChecks(initSegs)
  //        val lastSegWithChecks  = addLastCheck(lastSeg)
  //        def finalizerWithChecks(
  //          pfx: Option[Pattern.Match],
  //          segs: List[Macro.Match.Segment]
  //        ) = {
  //          val pfxFail  = !pfx.forall(_.isValid)
  //          val segsFail = !segs.forall(_.isValid)
  //          if (pfxFail || segsFail) {
  //            val pfxStream  = pfx.map(_.toStream).getOrElse(List())
  //            val segsStream = segs.flatMap(_.toStream)
  //            val stream     = pfxStream ++ segsStream
  ////            AST.Unexpected("invalid statement", stream)
  //            ???
  //          } else fin(pfx, segs)
  //        }
  //        __Definition__(
  //          backPatWithCheck,
  //          initSegsWithChecks,
  //          lastSegWithChecks,
  //          finalizerWithChecks
  //        )
  //      }
  //
  //    }
  //  }
  //
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  //// Space - unaware AST /////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  sealed trait SpaceUnawareShapeOf[T] extends ShapeOf[T]

  implicit def functorForSpaceUnawareShape[T]: Functor[SpaceUnawareShapeOf] =
    semi.functor
  implicit def offsetZipForSpaceUnawareShape[T]
    : OffsetZip[SpaceUnawareShapeOf, T] = t => t.map((0, _))

  //////////////////////////////////////////////////////////////////////////////
  /// Comment //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  sealed trait _Comment[T] extends SpaceUnawareShapeOf[T] with Phantom
  object Comment {
    val symbol = "#"

    type Disable    = _Disable[FixedAST]
    type SingleLine = _SingleLine[FixedAST]
    type MultiLine  = _MultiLine[FixedAST]

    case class _Disable[T](ast: T)                          extends _Comment[T]
    case class _SingleLine[T](text: String)                 extends _Comment[T]
    case class _MultiLine[T](off: Int, lines: List[String]) extends _Comment[T]

    //// Instances ////

    // Functor
    implicit def functorForDisableComment[T]: Functor[_Disable] =
      semi.functor
    implicit def functorForSingleLineComment[T]: Functor[_SingleLine] =
      semi.functor
    implicit def functorForMultiLineComment[T]: Functor[_MultiLine] =
      semi.functor

    // Repr
    implicit def reprForDisableComment[T: Repr.Of]: Repr.Of[_Disable[T]] =
      t => R + symbol + " " + t.ast
    implicit def reprForSingleLineComment[T]: Repr.Of[_SingleLine[T]] =
      t => R + symbol + symbol + t.text
    implicit def reprForMultiLineComment[T]: Repr.Of[_MultiLine[T]] =
      t => {
        val commentBlock = t.lines match {
          case Nil => Nil
          case line +: lines =>
            val indentedLines = lines.map { s =>
              if (s.forall(_ == ' ')) newline + s
              else newline + 1 + t.off + s
            }
            (R + line) +: indentedLines
        }
        R + symbol + symbol + commentBlock
      }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Import //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Import = _Import[FixedAST]
  case class _Import[T](path: List1[Cons]) extends SpaceUnawareShapeOf[T]

  object Import {
    def apply(path: List1[Cons]):            Import = _Import(path)
    def apply(head: Cons):                   Import = Import(head, List())
    def apply(head: Cons, tail: List[Cons]): Import = Import(List1(head, tail))
    def apply(head: Cons, tail: Cons*):      Import = Import(head, tail.toList)

    object implicits extends implicits
    trait implicits {
      implicit def functorForImport[T]: Functor[_Import] = semi.functor
      implicit def reprForImport[T]: Repr.Of[_Import[T]] =
        t => R + ("import " + t.path.map(_.repr.show()).toList.mkString("."))
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Mixfix //////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Mixfix = MixfixOf[FixedAST]
  case class MixfixOf[T](name: List1[Ident], args: List1[T])
      extends SpaceUnawareShapeOf[T]

  object Mixfix {
    object implicits extends implicits
    trait implicits {
      implicit def functorForMixfix[T]: Functor[MixfixOf] = semi.functor
      implicit def reprForMixfix[T: Repr.Of]: Repr.Of[MixfixOf[T]] =
        t => {
          val lastRepr =
            if (t.name.length - t.args.length > 0) List(R) else List()
          val argsRepr = t.args.toList.map(R + " " + _) ++ lastRepr
          val nameRepr = t.name.toList.map(Repr.of(_))
          R + (nameRepr, argsRepr).zipped.map(_ + _)
        }
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Group ///////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Group = GroupOf[FixedAST]
  case class GroupOf[T](body: Option[T]) extends SpaceUnawareShapeOf[T]

  object Group {
    def apply(body: Option[AST]): Group = Group(body)
    def apply(body: AST):         Group = Group(Some(body))
    def apply(body: SAST):        Group = Group(body.el)
    def apply():                  Group = Group(None)

    object implicits extends implicits
    trait implicits {
      implicit def functorForGroup[T]:       Functor[GroupOf]    = semi.functor
      implicit def reprForGroup[T: Repr.Of]: Repr.Of[GroupOf[T]] = R + _.body
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Def /////////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Def = DefOf[FixedAST]
  case class DefOf[T](name: Cons, args: List[T], body: Option[T])
      extends SpaceUnawareShapeOf[T]

  object Def {
    val symbol = "def"

    def apply(name: Cons):                  Def = Def(name, List())
    def apply(name: Cons, args: List[AST]): Def = Def(name, args, None)
    def apply(name: Cons, args: List[AST], body: Option[AST]): Def =
      DefOf(name, args, body)

    object implicits extends implicits
    trait implicits {
      implicit def functorForDef[T]: Functor[DefOf] = semi.functor
      implicit def reprForDef[T: Repr.Of]: Repr.Of[DefOf[T]] =
        t => R + symbol ++ t.name + t.args.map(R ++ _) + t.body
    }
  }

  //////////////////////////////////////////////////////////////////////////////
  //// Foreign /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  type Foreign = ForeignOf[FixedAST]
  case class ForeignOf[T](indent: Int, lang: String, code: List[String])
      extends SpaceUnawareShapeOf[T]

  object Foreign
//     {
//      val repr = {
//        val code2 = code.map(R + indent + _).mkString("\n")
//        R + "foreign " + lang + "\n" + code2
//      }
//      def setID(newID: ID)                 = copy(id = Some(newID))
//      def map(f: AST => AST)               = this
//      def mapWithOff(f: (Int, AST) => AST) = this
//    }
  //
  //  //////////////////////////////////////////////////////////////////////////////
  //  //// Zipper //////////////////////////////////////////////////////////////////
  //  //////////////////////////////////////////////////////////////////////////////
  //
  //  /**
  //    * This is just a stub implementation. It shows how zippers could be
  //    * implemented for AST.
  //    */
  //  sealed trait Zipper[Begin, End]
  //  object Zipper {
  //
  //    sealed trait Path[Begin, End] {
  //      val path: monocle.Optional[Begin, End]
  //    }
  //
  //    sealed trait Has { type Zipper[_] }
  //
  //    sealed trait Provider[Begin, End] {
  //      type Zipper
  //      def focus: Path[Begin, End] => Zipper
  //    }
  //    object Provider {
  //      sealed trait Inferred[Begin, End <: Has] extends Provider[Begin, End] {
  //        type Zipper = End#Zipper[Begin]
  //
  //      }
  //      sealed trait Terminated[Begin, End] extends Provider[Begin, End] {
  //        type Zipper = Terminator[Begin, End]
  //      }
  //
  //      implicit def default[S, T]: Terminated[S, T] =
  //        new Terminated[S, T] {
  //          val focus = Terminator(_)
  //        }
  //    }
  //  }
  //
  //  implicit def inferredZipperProvider[S, T <: Zipper.Has](
  //    implicit ev: Zipper.Path[S, T] => T#Zipper[S]
  //  ): Zipper.Provider.Inferred[S, T] = new Zipper.Provider.Inferred[S, T] {
  //    val focus = ev(_)
  //  }
  //
  //  def zipper[S, T](
  //    lens: Zipper.Path[S, T]
  //  )(implicit ev: Zipper.Provider[S, T]): ev.Zipper =
  //    ev.focus(lens)
  //
  //   case class Terminator[S, T](zipper: Zipper.Path[S, T])
  //      extends AST.Zipper[S, T]
  //
  //  implicit def ZipperTarget_List1[S, T]
  //    : Zipper.Provider[S, List1[T]] { type Zipper = List1Target[S, T] } =
  //    new Zipper.Provider[S, List1[T]] {
  //      type Zipper = List1Target[S, T]
  //      def focus = List1Target(_)
  //    }
  //
  //   case class List1Target[S, T](lens: AST.Zipper.Path[S, List1[T]])
  //      extends AST.Zipper[S, List1[T]] {
  //    def index(
  //      idx: Int
  //    )(implicit ev: Zipper.Provider[List1[T], T]): ev.Zipper =
  //      zipper(List1Zipper[S, T](lens, idx))
  //  }
  //
  //   case class List1Zipper[S, T](
  //    zipper: AST.Zipper.Path[S, List1[T]],
  //    idx: Int
  //  ) extends AST.Zipper.Path[List1[T], T] {
  //    val getOpt = (t: List1[T]) =>
  //      idx match {
  //        case 0 => Some(t.head)
  //        case i => t.tail.lift(i - 1)
  //      }
  //    val setOpt = (s: T) =>
  //      (t: List1[T]) =>
  //        idx match {
  //          case 0 => t.copy(head = s)
  //          case _ =>
  //            val i = idx - 1
  //            if ((i >= t.tail.length) || (i < 0)) t
  //            else {
  //              val (front, back) = t.tail.splitAt(i)
  //              val tail2         = front ++ (s :: back.tail)
  //              List1(t.head, tail2)
  //            }
  //        }
  //    val path = monocle.Optional[List1[T], T](getOpt)(setOpt)
  //  }
  //
  //  val z1 = Module.Zipper.lines.index(5).offset.zipper
}
