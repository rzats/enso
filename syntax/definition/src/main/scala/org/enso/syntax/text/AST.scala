package org.enso.syntax.text

import cats.data.NonEmptyList
import org.enso.data.ADT
import org.enso.data.Shifted
import org.enso.data.Tree
import org.enso.syntax.text.ast.Repr.R
import org.enso.syntax.text.ast.Repr
import org.enso.syntax.text.ast.opr

sealed trait AST extends AST.Symbol

object AST {

  ///////////////////
  //// Reexports ////
  ///////////////////

  type Assoc = opr.Assoc

  val Assoc = opr.Assoc
  val Prec  = opr.Prec

  ////////////////////
  //// Definition ////
  ////////////////////

  trait Symbol extends Repr.Provider {
    def span:   Int    = repr.span
    def show(): String = repr.show()
  }

  /////////////////////
  //// Conversions ////
  /////////////////////

  implicit def fromString(str: String): AST =
    fromStringRaw(str) match {
      case opr: Opr => App.Sides(opr)
      case any      => any
    }

  def fromStringRaw(str: String): AST = {
    if (str == "") throw new Error("Empty literal")
    if (str == "_") Blank
    else if (str.head.isLower) Var(str)
    else if (str.head.isUpper) Cons(str)
    else Opr(str)
  }

  implicit final private class OptAST(val self: Option[AST]) extends Symbol {
    val repr = self.map(_.repr).getOrElse(Repr())
  }

  /////////////////
  //// Invalid ////
  /////////////////

  sealed trait Invalid extends AST

  final case class Unrecognized(str: String) extends Invalid {
    val repr = str
  }

  ////////////////////
  //// Identifier ////
  ////////////////////

  abstract class Named(name: String) extends Repr.Provider {
    val repr = name
  }

  trait Literal extends AST
  trait Ident   extends Named with Literal

  object Ident {
    final case class InvalidSuffix(elem: Ident, suffix: String)
        extends AST.Invalid {
      val repr = R + elem + suffix
    }
    implicit def fromString(str: String): Ident = {
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
    private val desc = Opr.Info.of(name)
    val prec         = desc.prec
    val assoc        = desc.assoc
  }
  object Opr {
    val app: Opr = Opr(" ")

    case class Info(prec: Int, assoc: Assoc)
    object Info {
      val map: Map[String, Info] = Prec.map.map {
        case (name, prec) => name -> Info(prec, Assoc.of(name))
      }
      def of(op: String) =
        map.getOrElse(op, Info(Prec.hierarchy.length, Assoc.of(op)))
    }

    implicit def fromString(str: String): Opr = Opr(str)
  }
  final case class Mod(name: String) extends Named(name) with Ident {
    override val repr = name + '='
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
      opr: Opr,
      rightOff: Int,
      rightArg: AST
    ): Infix = Infix(leftArg, leftOff, opr, rightOff, rightArg)

    final case class Left(arg: AST, off: Int, op: Opr) extends AST {
      val repr = R + arg + off + op
    }

    final case class Right(opr: Opr, off: Int, arg: AST) extends AST {
      val repr = R + opr + off + arg
    }

    final case class Sides(opr: Opr) extends AST {
      val repr = R + opr
    }

    final case class Infix(
      leftArg: AST,
      leftOff: Int,
      opr: Opr,
      rightOff: Int,
      rightArg: AST
    ) extends AST {
      val repr = R + leftArg + leftOff + opr + rightOff + rightArg
    }
  }

  //////////////////////////////////////////////////////////////////////////////

  case class Mixfix(segments: Shifted.List1[Mixfix.Segment.Class]) extends AST {
    val repr = R + segments.map(_.repr)
  }
  object Mixfix {

    case class Segment[T: Repr.Of](tp: Segment.Type[T], head: AST, body: T)
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
      segments: Shifted.List1[Unmatched.Segment],
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
          val codes = ADT.constructors[Character]
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
          val codes = ADT.constructors[Control]
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
      def toOptional: Line =
        Line(Some(elem), offset)
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
