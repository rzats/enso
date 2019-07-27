package org.enso.syntax.text
import cats.data.NonEmptyList
import org.enso.data.Tree

sealed trait AST extends AST.Symbol {
  import AST._

  private def smartApp(off: Int)(r: AST): AST = (this, r) match {
    case (l, r: App.Sides) => App.Left(l, off, r.operator)
    case (l: App.Sides, r) => App.Right(l.operator, off, r)
    case (l, r)            => App(l, off, r)
  }

  def $(t: AST)    = smartApp(0)(t)
  def $_(t: AST)   = smartApp(1)(t)
  def $__(t: AST)  = smartApp(2)(t)
  def $___(t: AST) = smartApp(3)(t)

  def $$(t: AST)    = smartApp(0)(t)
  def $$_(t: AST)   = smartApp(1)(t)
  def $$__(t: AST)  = smartApp(2)(t)
  def $$___(t: AST) = smartApp(3)(t)
}

object AST {

  case class Shifted[+T: ReprOf](off: Int, el: T) extends HasRepr {
    val repr = R + off + implicitly[ReprOf[T]].reprOf(el)
    def map[S](f: T => S)(implicit ev: ReprOf[S]): Shifted[S] =
      Shifted(off, f(el))
  }
  object Shifted {
    def apply[T: ReprOf](el: T): Shifted[T] = Shifted(0, el)
  }

  case class ShiftedList1[T: ReprOf](head: T, tail: List[Shifted[T]])
      extends HasRepr {
    val repr = R + head + tail.map(_.repr)

    def map[S: ReprOf](f: T => S): ShiftedList1[S] =
      ShiftedList1(f(head), tail.map(_.map(f)))

    def prepend(t: T, off: Int): ShiftedList1[T] =
      ShiftedList1(t, Shifted(off, head) :: tail)

    def prepend(t: Shifted[T]): ShiftedList1[T] =
      ShiftedList1(t.el, Shifted(t.off, head) :: tail)

    def toList(): List[Shifted[T]] =
      Shifted(0, head) :: tail

    def +(that: ShiftedList1[T]): ShiftedList1[T] =
      ShiftedList1(head, tail ++ that.toList())

    def +(that: List[Shifted[T]]): ShiftedList1[T] =
      ShiftedList1(head, tail ++ that)

    def :+(that: Shifted[T]): ShiftedList1[T] =
      ShiftedList1(head, tail :+ that)
  }
  object ShiftedList1 {
    def apply[T: ReprOf](head: T): ShiftedList1[T] = ShiftedList1(head, Nil)

    implicit def fromTuple[T: ReprOf](
      t: (T, List[Shifted[T]])
    ): ShiftedList1[T] = ShiftedList1(t._1, t._2)
  }

  //////////////////////////////////

  sealed trait Invalid extends AST

  /////////////////////////////////

  trait Association
  case object Left  extends Association
  case object Right extends Association

  def assocOf(op: String): Association = {
    val applicativePat = "<?[+*$]>?".r
    def isApplicative(s: String) = s match {
      case applicativePat() => s.length > 1
      case _                => false
    }
    def assocVal(c: Char) = c match {
      case ',' => -1
      case '<' => -1
      case '>' => 1
      case _   => 0
    }
    if (isApplicative(op)) Left
    else if (op.map(assocVal).sum >= 0) Left
    else Right
  }

  val precHierarchy = List(
    List("->", "<-"),
    List("~>", "<~"),
    List("|"),
    List("&"),
    List("=", "!", "?", "~"),
    List("<*", "<*>", "*>", "<$", "<$>", "$>", "<+", "<+>", "+>"),
    List("<", ">"),
    List(":", ","),
    List("+", "-"),
    List("*", "/", "\\", "%"),
    List("^"),
    List("."),
    List(" ")
  )

  case class Desc(prec: Int, assoc: Association)

  val descMap = precHierarchy.zipWithIndex.flatMap {
    case (ops, prec) => ops.map(op => op -> Desc(prec, assocOf(op)))
  }.toMap

  def descOf(op: String) =
    descMap.getOrElse(op, Desc(precHierarchy.length, assocOf(op)))

  //////

  import reflect.runtime.universe.TypeTag

  def getAllSeleadObjects[T](implicit ttag: TypeTag[T]) = {
    val subs = ttag.tpe.typeSymbol.asClass.knownDirectSubclasses
    subs.map { symbol =>
      val module = reflect.runtime.currentMirror.staticModule(symbol.fullName)
      val clazz  = reflect.runtime.currentMirror.reflectModule(module)
      clazz.instance.asInstanceOf[T]
    }
  }

  /////////////////////
  //// Abstraction ////
  /////////////////////

  trait Convertible[-Source, +Target] {
    def convert(source: Source): Target
  }

  trait Showable {
    def show(): String
  }

  trait Spanned {
    def span: Int
  }

  //////////////
  //// Repr ////
  //////////////

  trait HasRepr {
    val repr: Repr
  }

  sealed trait Repr extends HasRepr {
    import Repr._

    val repr = this
    val span: Int

    def show(out: StringBuilder): Unit

    def +[T: ReprOf](that: T) =
      Seq(this, implicitly[ReprOf[T]].reprOf(that))
  }

  object Repr {
    def apply():                Repr = Empty()
    def apply[T: ReprOf](t: T): Repr = implicitly[ReprOf[T]].reprOf(t)

    case class Empty() extends Repr {
      val span                     = 0
      def show(out: StringBuilder) = {}
    }

    case class Letter(char: Char) extends Repr {
      val span                     = 1
      def show(out: StringBuilder) = out += char
    }

    case class Text(str: String) extends Repr {
      val span                     = str.length
      def show(out: StringBuilder) = out ++= str
    }

    final case class Seq(first: Repr, second: Repr) extends Repr {
      val span = first.span + second.span
      override def show(out: StringBuilder) = {
        first.show(out)
        second.show(out)
      }
    }

    implicit def stringToRepr(a: String): Repr = Repr(a)
    implicit def charToRepr(a: Char):     Repr = Repr(a)
  }

  val R = Repr.Empty()

  ///// Type Class ////

  trait ReprOf[-T] {
    def reprOf(a: T): Repr
  }
  def reprOf[T](t: T)(implicit ev: ReprOf[T]) = ev.reprOf(t)

  implicit val HasRepr_0: ReprOf[Unit]    = _ => Repr.Empty()
  implicit val HasRepr_1: ReprOf[String]  = Repr.Text(_)
  implicit val HasRepr_2: ReprOf[Int]     = i => Repr.Text(" " * i)
  implicit val HasRepr_3: ReprOf[Char]    = Repr.Letter(_)
  implicit val HasRepr_4: ReprOf[Repr]    = identity(_)
  implicit val HasRepr_5: ReprOf[HasRepr] = _.repr
  implicit def HasRepr_6[T: ReprOf]: ReprOf[List[T]] =
    _.foldLeft(Repr.Empty(): Repr)((a, b) => Repr.Seq(a, reprOf(b)))
  implicit def HasRepr_7[T: ReprOf]: ReprOf[NonEmptyList[T]] =
    _.foldLeft(Repr.Empty(): Repr)((a, b) => Repr.Seq(a, reprOf(b)))

  ////////////////
  //// Symbol ////
  ////////////////

  trait Symbol extends HasRepr with Spanned with Showable {
    def to[A](implicit converter: Convertible[this.type, A]): A =
      converter.convert(this)
    def span: Int = repr.span
    def show() = {
      val bldr = new StringBuilder()
      repr.show(bldr)
      bldr.result()
    }
  }

  /////////////
  //// AST ////
  /////////////

  implicit final class _OptionAST_(val self: Option[AST]) extends Symbol {
    val repr = self.map(_.repr).getOrElse(Repr())
  }

  implicit def ReprOfOption[T: ReprOf]: ReprOf[Option[T]] =
    _.map(reprOf(_)).getOrElse(Repr())

  // FIXME: Why this is needed?
  implicit def ReprOfOptionAST: ReprOf[Option[AST]] =
    _.map(_.repr).getOrElse(Repr())

  //// Unrecognized ////

  final case class Unrecognized(str: String) extends AST.Invalid {
    val repr = str
  }

  //// Identifiers ////

  abstract class Named(name: String) extends HasRepr {
    val repr = name
  }

  trait Literal extends AST
  trait Ident   extends Named with Literal

  object Ident {
    final case class InvalidSuffix(elem: Ident, suffix: String)
        extends AST.Invalid {
      val repr = R + elem + suffix
    }
    implicit def stringToIdentifier(str: String): Ident = {
      if (str == "") throw new Error("Empty literal")
      if (str == "_") Blank
      else if (str.head.isLower) Var(str)
      else if (str.head.isUpper) Cons(str)
      else Opr(str)
    }
  }

  final case object Blank             extends Named("_") with Ident
  final case class Var(name: String)  extends Named(name) with Ident
  final case class Cons(name: String) extends Named(name) with Ident
  final case class Opr(name: String) extends Named(name) with Ident {
    private val desc = descOf(name)
    val prec         = desc.prec
    val assoc        = desc.assoc
  }
  object Opr {
    val app: Opr = Opr(" ")
    implicit def fromString(str: String): Opr = Opr(str)
  }
  final case class Modifier(name: String) extends Named(name) with Ident {
    override val repr = name + '='
  }

  implicit def stringToAST(str: String): AST = {
    if (str == "") throw new Error("Empty literal")
    if (str == "_") Blank
    else if (str.head.isLower) Var(str)
    else if (str.head.isUpper) Cons(str)
    else App.Sides(Opr(str))
  }

  def stringToRawAST(str: String): AST = {
    if (str == "") throw new Error("Empty literal")
    if (str == "_") Blank
    else if (str.head.isLower) Var(str)
    else if (str.head.isUpper) Cons(str)
    else Opr(str)
  }

  //////////////////////////////////////////////////////////////////////////////

  final case class App(func: AST, off: Int, arg: AST) extends AST {
    val repr = R + func + off + arg
  }
  object App {
    def apply(func: AST, arg: AST):         App   = App(func, 1, arg)
    def apply(op: Opr, off: Int, arg: AST): Right = Right(op, off, arg)
    def apply(op: Opr, arg: AST):           Right = Right(op, 1, arg)
    def apply(arg: AST, off: Int, op: Opr): Left  = Left(arg, off, op)
    def apply(arg: AST, op: Opr):           Left  = Left(arg, 1, op)
    def apply(
      leftArg: AST,
      leftOff: Int,
      operator: Opr,
      rightOff: Int,
      rightArg: AST
    ): Infix = Infix(leftArg, leftOff, operator, rightOff, rightArg)

    final case class Infix(
      leftArg: AST,
      leftOff: Int,
      operator: Opr,
      rightOff: Int,
      rightArg: AST
    ) extends AST {
      val repr = R + leftArg + leftOff + operator + rightOff + rightArg
    }

    final case class Right(operator: Opr, off: Int, arg: AST) extends AST {
      val repr = R + operator + off + arg
    }

    final case class Left(arg: AST, off: Int, op: Opr) extends AST {
      val repr                  = R + arg + off + op
      override def $(t: AST)    = Infix(arg, off, op, 0, t)
      override def $_(t: AST)   = Infix(arg, off, op, 1, t)
      override def $__(t: AST)  = Infix(arg, off, op, 2, t)
      override def $___(t: AST) = Infix(arg, off, op, 3, t)
    }

    final case class Sides(operator: Opr) extends AST {
      val repr = R + operator
    }

  }

  //////////////////////////////////////////////////////////////////////////////

  case class Mixfix(segments: ShiftedList1[Mixfix.Segment.Class]) extends AST {
    val repr = R + segments.map(_.repr)
  }
  object Mixfix {

    case class Segment[T: ReprOf](tp: Segment.Type[T], head: AST, body: T)
        extends Segment.Class {
      val repr = R + head + body
    }
    object Segment {

      trait Class extends Symbol
      type Any = Segment[_]

      trait Type[+T]
      object Type {
        type Any = Type[_]
      }

      final case class Empty() extends Type[Unit]
      object Empty {
        case class NonEmpty(head: AST, body: Shifted[AST])
            extends Class
            with Invalid {
          val repr = R + head + body
        }
      }

      final case class Expr() extends Type[Option[Shifted[AST]]]

      final case class Expr1() extends Type[Shifted[AST]]
      object Expr1 {
        case class Empty(head: AST) extends Class with Invalid {
          val repr = R + head
        }
      }

      def apply(head: AST, body: Option[Shifted[AST]]): Segment[_] =
        new Segment(Expr(), head, body)

      def apply(head: AST): Segment[_] =
        new Segment(Empty(), head, ())

    }

    case class Unmatched(
      segments: ShiftedList1[Unmatched.Segment],
      possiblePaths: Tree[AST, Unit]
    ) extends AST {
      val repr = R + segments.map(_.repr)
    }
    object Unmatched {
      case class Segment(head: AST, body: Option[Shifted[AST]]) extends Symbol {
        val repr = R + head + body
      }
    }

    case class Definition(segments: NonEmptyList[Definition.Input])
    object Definition {
      type Input = (AST, Segment.Type[_])
      def apply(t1: Input, ts: Input*): Definition =
        Definition(NonEmptyList(t1, ts.to[List]))
    }
  }

  //////////////////////////////////////////////////////////////////////////////

//  final case class Group(leftOff: Int, body: Option[AST], rightOff: Int)
//      extends AST {
//    val repr = Repr('(') + leftOff + body + rightOff + ')'
//  }
//
//  object Group {
//    def apply():                       Group = Group(0, None, 0)
//    def apply(l: Int):                 Group = Group(l, None, 0)
//    def apply(b: AST):                 Group = Group(0, Some(b), 0)
//    def apply(l: Int, b: AST):         Group = Group(l, Some(b), 0)
//    def apply(b: AST, r: Int):         Group = Group(0, Some(b), r)
//    def apply(l: Int, b: AST, r: Int): Group = Group(l, Some(b), r)
//
//    final case object UnmatchedClose extends AST.Invalid {
//      val repr = ')'
//    }
//
//    final case class Unclosed(leftOff: Int, body: Option[AST])
//        extends AST.Invalid {
//      val repr = R + '(' + leftOff + body
//    }
//
//    object Unclosed {
//      def apply():               Unclosed = Unclosed(0, None)
//      def apply(b: AST):         Unclosed = Unclosed(0, Some(b))
//      def apply(l: Int, b: AST): Unclosed = Unclosed(l, Some(b))
//    }
//  }

  //////////////////////////////////////////////////////////////////////////////

  final case class Number(base: Option[String], int: String) extends AST {
    val repr = base.map(_ + "_").getOrElse("") + int
  }

  object Number {
    def apply(i: Int):               Number = Number(i.toString)
    def apply(i: String):            Number = Number(None, i)
    def apply(b: String, i: String): Number = Number(Some(b), i)
    def apply(b: Int, i: String):    Number = Number(b.toString, i)
    def apply(b: String, i: Int):    Number = Number(b, i.toString)
    def apply(b: Int, i: Int):       Number = Number(b.toString, i.toString)

    final case class DanglingBase(base: String) extends AST.Invalid {
      val repr = base + '_'
    }
  }

  implicit def IntToNumber(int: Int): Number = Number(int)

  //////////////////////////////////////////////////////////////////////////////

  final case class Text(quoteSize: Text.QuoteSize, segments: List[Text.Segment])
      extends AST {
    val repr = R + quoteSize + segments + quoteSize

    def prepend(segment: Text.Segment) =
      this.copy(segments = segment :: segments)

    def prependMergeReversed(segment: Text.Segment) =
      (segment, segments) match {
        case (Text.Segment.Plain(n), Text.Segment.Plain(t) :: ss) =>
          this.copy(segments = Text.Segment.Plain(t + n) :: ss)
        case _ => this.copy(segments = segment :: segments)
      }
  }

  object Text {
    trait QuoteSize extends Symbol

    case object SingleQuote extends QuoteSize {
      val repr = '\''
    }

    case object TripleQuote extends QuoteSize {
      val repr = "'''"
    }

    def apply():                          Text = Text(SingleQuote, Nil)
    def apply(q: QuoteSize):              Text = Text(q, Nil)
    def apply(q: QuoteSize, s: Segment*): Text = Text(q, s.to[List])
    def apply(s: List[Segment]):          Text = Text(SingleQuote, s)
    def apply(s: Segment*):               Text = Text(s.to[List])

    final case class Unclosed(text: Text) extends AST.Invalid {
      val repr = R + text.quoteSize + text.segments
    }

    trait Segment extends Symbol {
      def +:(text: Text) = text.prepend(this)
    }

    object Segment {
      final case class Plain(value: String) extends Segment {
        val repr = value
      }

      final case class Interpolated(value: Option[AST]) extends Segment {
        val repr = R + '`' + value + '`'
      }
      //FIXME: Why it does not work?
//      object Interpolated {
//        def apply(t: AST): Interpolated = Interpolated(Some(t))
//      }

      implicit def stringToPlain(str: String): Plain = Plain(str)

      trait Escape extends Segment
      object Escape {

        abstract class Simple(val code: Int) extends Escape {
          val name = toString()
          val repr = '\\' + name
        }

        case object Slash extends Escape {
          val repr = "\\\\"
        }

        case object Quote extends Escape {
          val repr = "\\'"
        }

        case object RawQuote extends Escape {
          val repr = "\\\""
        }

        case class Number(int: Int) extends Escape {
          val repr = '\\' + int.toString
        }

        case class Invalid(str: String) extends Escape with AST.Invalid {
          val repr = '\\' + str
        }

        // Reference: https://en.wikipedia.org/wiki/String_literal
        sealed trait Unicode extends Escape
        object Unicode {
          abstract class U(val pfx: String, val sfx: String) extends Unicode {
            val digits: String
            val repr = R + "\\" + pfx + digits + sfx

          }
          final case class U16 private (digits: String) extends U("u", "")
          final case class U32 private (digits: String) extends U("U", "")
          final case class U21 private (digits: String) extends U("u{", "}")
          final case class InvalidU16 private (digits: String)
              extends U("u", "")
              with AST.Invalid
          final case class InvalidU32 private (digits: String)
              extends U("U", "")
              with AST.Invalid
          final case class InvalidU21 private (digits: String)
              extends U("u{", "}")
              with AST.Invalid

          object Validator {
            val hexChars = (('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9')).toSet
            def isHexChar(char: Char) =
              hexChars.contains(char)
          }

          object U16 {
            def apply(digits: String): Unicode =
              if (validate(digits)) new U16(digits) else InvalidU16(digits)
            def validate(digits: String) = {
              import Validator._
              val validLength = digits.length == 4
              val validChars  = digits.map(isHexChar).forall(identity)
              validLength && validChars
            }
          }
          object U32 {
            def apply(digits: String): Unicode =
              if (validate(digits)) new U32(digits) else InvalidU32(digits)
            def validate(digits: String) = {
              import Validator._
              val validLength = digits.length == 8
              val validPrefix = digits.startsWith("00")
              val validChars  = digits.map(isHexChar).forall(identity)
              validLength && validPrefix && validChars
            }
          }
          object U21 {
            def apply(digits: String): Unicode =
              if (validate(digits)) new U21(digits) else InvalidU21(digits)
            def validate(digits: String) = {
              import Validator._
              val validLength = digits.length >= 1 && digits.length <= 6
              val validChars  = digits.map(isHexChar).forall(identity)
              validLength && validChars
            }
          }
        }

        // Reference: https://en.wikipedia.org/wiki/String_literal
        sealed trait Character extends Escape
        object Character {
          case object a extends Simple('\u0007') with Character
          case object b extends Simple('\u0008') with Character
          case object f extends Simple('\u000C') with Character
          case object n extends Simple('\n') with Character
          case object r extends Simple('\r') with Character
          case object t extends Simple('\u0009') with Character
          case object v extends Simple('\u000B') with Character
          case object e extends Simple('\u001B') with Character
          val codes = getAllSeleadObjects[Character]
        }

        // Reference: https://en.wikipedia.org/wiki/Control_character
        sealed trait Control extends Escape
        object Control {
          case object NUL extends Simple(0x00) with Control
          case object SOH extends Simple(0x01) with Control
          case object STX extends Simple(0x02) with Control
          case object ETX extends Simple(0x03) with Control
          case object EOT extends Simple(0x04) with Control
          case object ENQ extends Simple(0x05) with Control
          case object ACK extends Simple(0x06) with Control
          case object BEL extends Simple(0x07) with Control
          case object BS  extends Simple(0x08) with Control
          case object TAB extends Simple(0x09) with Control
          case object LF  extends Simple(0x0A) with Control
          case object VT  extends Simple(0x0B) with Control
          case object FF  extends Simple(0x0C) with Control
          case object CR  extends Simple(0x0D) with Control
          case object SO  extends Simple(0x0E) with Control
          case object SI  extends Simple(0x0F) with Control
          case object DLE extends Simple(0x10) with Control
          case object DC1 extends Simple(0x11) with Control
          case object DC2 extends Simple(0x12) with Control
          case object DC3 extends Simple(0x13) with Control
          case object DC4 extends Simple(0x14) with Control
          case object NAK extends Simple(0x15) with Control
          case object SYN extends Simple(0x16) with Control
          case object ETB extends Simple(0x17) with Control
          case object CAN extends Simple(0x18) with Control
          case object EM  extends Simple(0x19) with Control
          case object SUB extends Simple(0x1A) with Control
          case object ESC extends Simple(0x1B) with Control
          case object FS  extends Simple(0x1C) with Control
          case object GS  extends Simple(0x1D) with Control
          case object RS  extends Simple(0x1E) with Control
          case object US  extends Simple(0x1F) with Control
          case object DEL extends Simple(0x7F) with Control
          val codes = getAllSeleadObjects[Control]
        }
      }
    }
  }

  //// Block ////

  final case class Block(
    indent: Int,
    emptyLines: List[Int],
    firstLine: Line.Required,
    lines: List[Line]
  ) extends AST {
    val repr = {
      val headRepr       = R + '\n'
      val emptyLinesRepr = emptyLines.map(R + indent + _ + "\n")
      val firstLineRepr  = R + indent + firstLine
      val linesRepr      = lines.map(R + '\n' + indent + _)
      headRepr + emptyLinesRepr + firstLineRepr + linesRepr
    }
  }

  object Block {
    final case class InvalidIndentation(block: Block) extends AST.Invalid {
      val repr = R + block
    }
  }

  final case class Line(elem: Option[AST], offset: Int) extends Symbol {
    val repr = R + elem + offset
    def map(f: AST => AST): Line =
      Line(elem.map(f), offset)
  }

  object Line {
    def apply():            Line = Line(None, 0)
    def apply(offset: Int): Line = Line(None, offset)

    final case class Required(elem: AST, offset: Int) extends Symbol {
      val repr = R + elem + offset
    }

    implicit object Required_to_Optional extends Convertible[Required, Line] {
      def convert(src: Required): Line = Line(Some(src.elem), src.offset)
    }
  }

  //// Unit ////

  final case class Module(firstLine: Line, lines: List[Line]) extends AST {
    val repr = R + firstLine + lines.map(R + '\n' + _)

    def map(f: Line => Line): Module =
      Module(f(firstLine), lines.map(f))
  }

  object Module {
    def apply(l: Line):            Module = Module(l, Nil)
    def apply(l: Line, ls: Line*): Module = Module(l, ls.to[List])
  }

}
