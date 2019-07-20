package org.enso.parser

import scala.annotation.tailrec
import scala.collection.immutable.Map
import cats.data.NonEmptyList
import org.enso.parser.Ops.ExprList

object AST {

  trait Association
  case object Left  extends Association
  case object Right extends Association
//  case object None  extends Association

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
    else if (op.map(assocVal(_)).sum >= 0) Left
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

  import reflect.runtime.universe._

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

    def +[T](that: T)(implicit ev: ReprOf[T]) =
      Seq(this, ev.reprOf(that))
  }

  object Repr {
    def apply():                                Repr = Empty()
    def apply[T](t: T)(implicit ev: ReprOf[T]): Repr = ev.reprOf(t)

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

  implicit val HasRepr_1: ReprOf[String]  = Repr.Text(_)
  implicit val HasRepr_2: ReprOf[Int]     = i => Repr.Text(" " * i)
  implicit val HasRepr_3: ReprOf[Char]    = Repr.Letter(_)
  implicit val HasRepr_4: ReprOf[Repr]    = identity(_)
  implicit val HasRepr_5: ReprOf[HasRepr] = _.repr
  implicit def HasRepr_6[T](implicit ev: ReprOf[T]): ReprOf[List[T]] =
    _.foldLeft(Repr.Empty(): Repr)((a, b) => Repr.Seq(a, ev.reprOf(b)))

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

  trait AST extends Symbol {
    def $(arg: AST)    = App(this, 0, arg)
    def $_(arg: AST)   = App(this, 1, arg)
    def $__(arg: AST)  = App(this, 2, arg)
    def $___(arg: AST) = App(this, 3, arg)
  }

  trait Invalid
  trait InvalidAST extends AST with Invalid

  implicit final class _OptionAST_(val self: Option[AST]) extends Symbol {
    val repr = self.map(_.repr).getOrElse(Repr())
  }

  implicit def ReprOfOption[T](implicit ev: ReprOf[T]): ReprOf[Option[T]] =
    _.map(ev.reprOf(_)).getOrElse(Repr())

  // FIXME: Why this is needed?
  implicit def ReprOfOptionAST: ReprOf[Option[AST]] =
    _.map(_.repr).getOrElse(Repr())

  //// Unrecognized ////

  final case class Unrecognized(str: String) extends InvalidAST {
    val repr = str
  }

  //// Identifiers ////

  abstract class Identifier(name: String) extends AST {
    val repr = name
  }

  object Identifier {
    final case class InvalidSuffix(elem: Identifier, suffix: String)
        extends InvalidAST {
      val repr = R + elem + suffix
    }
  }

  final case object Wildcard          extends Identifier("_")
  final case class Var(name: String)  extends Identifier(name)
  final case class Cons(name: String) extends Identifier(name)
  final case class Operator(name: String) extends Identifier(name) {
    private val desc = descOf(name)
    val prec         = desc.prec
    val assoc        = desc.assoc
  }
  final case class Modifier(name: String) extends Identifier(name) {
    override val repr = name + '='
  }

  implicit def stringToIdentifier(str: String): Identifier = {
    if (str == "") throw new Error("Empty literal")
    if (str == "_") Wildcard
    else if (str.head.isLower) Var(str)
    else if (str.head.isUpper) Cons(str)
    else Operator(str)
  }

  //// App ////

  final case class App(func: AST, off: Int, arg: AST) extends AST {
    val repr = R + func + off + arg
  }
  object App {
    def apply(func: AST, arg: AST): App = new App(func, 1, arg)
  }

  final case class InfixApp(
    leftArg: AST,
    leftOff: Int,
    operator: Operator,
    rightOff: Int,
    rightArg: AST
  ) extends AST {
    val repr = R + leftArg + leftOff + operator + rightOff + rightArg
  }

  final case class SectionLeft(operator: Operator, off: Int, arg: AST)
      extends AST {
    val repr = R + operator + off + arg
  }

  final case class SectionRight(arg: AST, off: Int, operator: Operator)
      extends AST {
    val repr = R + arg + off + operator
  }

  final case class Section(operator: Operator) extends AST {
    val repr = R + operator
  }

  //// Group ////

  final case class Group(leftOff: Int, body: Option[AST], rightOff: Int)
      extends AST {
    val repr = Repr('(') + leftOff + body + rightOff + ')'
  }

  object Group {
    def apply():                       Group = Group(0, None, 0)
    def apply(l: Int):                 Group = Group(l, None, 0)
    def apply(b: AST):                 Group = Group(0, Some(b), 0)
    def apply(l: Int, b: AST):         Group = Group(l, Some(b), 0)
    def apply(b: AST, r: Int):         Group = Group(0, Some(b), r)
    def apply(l: Int, b: AST, r: Int): Group = Group(l, Some(b), r)

    final case object UnmatchedClose extends InvalidAST {
      val repr = ')'
    }

    final case class Unclosed(leftOff: Int, body: Option[AST])
        extends InvalidAST {
      val repr = R + '(' + leftOff + body
    }

    object Unclosed {
      def apply():               Unclosed = Unclosed(0, None)
      def apply(b: AST):         Unclosed = Unclosed(0, Some(b))
      def apply(l: Int, b: AST): Unclosed = Unclosed(l, Some(b))
    }

  }

  //// Number ////

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

    final case class DanglingBase(base: String) extends InvalidAST {
      val repr = base + '_'
    }
  }

  implicit def IntToNumber(int: Int): Number = Number(int)

  //////////////
  //// Text ////
  //////////////

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

    final case class Unclosed(text: Text) extends InvalidAST {
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
    final case class InvalidIndentation(block: Block) extends InvalidAST {
      val repr = R + block
    }
  }

  final case class Line(elem: Option[AST], offset: Int) extends Symbol {
    val repr = R + elem + offset
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
  }

  object Module {
    def apply(l: Line):            Module = Module(l, Nil)
    def apply(l: Line, ls: Line*): Module = Module(l, ls.to[List])
  }

  /////////////////////////////////////////////////////
  /////////////////////////////////////////////////////
  /////////////////////////////////////////////////////

}

object Ops {
  import AST._

  sealed trait Compare
  case object LT extends Compare
  case object GT extends Compare
  case object EQ extends Compare

  def compare[T: Ordering](a: T, b: T): Compare = {
    if (implicitly[Ordering[T]].lt(a, b)) LT
    else if (implicitly[Ordering[T]].gt(a, b)) GT
    else EQ
  }

  def cross(as: List[String], bs: List[String]) =
    for { a <- as; b <- bs } yield a + b

  case class NonSpacedSegment(expr: NonEmptyList[AST])
  case class SpacedSegment(segs: NonEmptyList[NonSpacedSegment])

  def partitionExprToSpaceGroups(t: AST): SpacedSegment = {
    @tailrec
    def go(
      t: AST,
      current: List[AST],
      out: List[NonSpacedSegment]
    ): NonEmptyList[NonSpacedSegment] = {
      def out2(t: AST) = NonSpacedSegment(NonEmptyList(t, current))
      t match {
        case AST.App(fn, off, arg) =>
          if (off > 0) go(fn, arg :: current, out)
          else go(fn, Nil, out2(arg) :: out)
        case _ => NonEmptyList(out2(t), out)
      }
    }
    SpacedSegment(go(t, Nil, Nil))
  }

  case class OpDesc(op: Operator, desc: Desc)

  trait OpList
  trait ExprList

  case class ExprNode(expr: AST, tail: OpList)  extends ExprList
  case class OpNode(op: OpDesc, tail: ExprList) extends OpList
  case object Empty                             extends ExprList with OpList

  def add(seg: NonEmptyList[AST]): AST = {
    seg.head match {
      case _: Operator => addOp(seg, Empty)
      case _           => addExpr(seg, Empty)
    }
  }

  val appOp = Operator(" ")

  def rebuildAssocExpr(seg: NonEmptyList[AST]) =
    go(seg.tail, NonEmptyList(seg.head, Nil))

  case class RebuildAssocExprInput(seg: List[AST], stack: NonEmptyList[AST]) {
    override def toString() =
      "\nseg: " + seg.toString + "\nstack: " + stack.toString
  }

  implicit def RebuildAssocExprInputFromTuple(
    tup: (List[AST], NonEmptyList[AST])
  ): RebuildAssocExprInput =
    RebuildAssocExprInput(tup._1, tup._2)

  def astToOp(ast: AST) = ast match {
    case ast: Operator => ast
    case _             => appOp
  }

  // format: off
  @tailrec
  def go(inp: RebuildAssocExprInput): AST = inp.seg match {
    case Nil => combine2(inp.stack)
    case seg1 :: seg2_ =>
      
      val shift  = (seg2_, seg1 :: inp.stack)
      val reduce = (inp.seg, reduce2(inp.stack))
      
      def handleOp(ast1: AST, ast2: AST) = {
        val op1 = astToOp(ast1)
        val op2 = astToOp(ast2)
        compare(op1.prec, op2.prec) match {
          case GT => shift
          case LT => reduce
          case EQ => (op1.assoc, op2.assoc) match {
            case (Left, Left) => reduce
            case _            => shift
          }
        }
      }

      inp.stack.head match {
        case stack1: Operator => seg1 match {
          case seg1:Operator => go(handleOp(seg1,stack1))
          case _             => go(shift)
        }
        case _ => inp.stack.tail match {
          case Nil         => go(shift)
          case stack2 :: _ => go(handleOp(seg1, stack2))
        }
      }
    }

  //TODO: right assoc, operator after operator (+ <$> ...)
  def reduceOps(seg1: AST, seg2_ : List[AST], stack: NonEmptyList[AST]) = {}

//  @tailrec
  def reduce2(stack: NonEmptyList[AST]): NonEmptyList[AST] = {
    println(s"\n>> reduce: $stack")
    stack.head match {
      case el1: Operator =>
        stack.tail match {
          case Nil => NonEmptyList(Section(el1), Nil)
          case (el2: Operator) :: el3_ => 
            reduce2(NonEmptyList(Section(el1), el2 :: el3_))
          case el2 :: el3_ =>
            NonEmptyList(SectionRight(el2, 0, el1), el3_)
        }
      case el1 =>
        stack.tail match {
          case Nil                                        => stack
          case (el2: Operator) :: (el3: Operator) :: el4_ =>
            ??? // reduce2(NonEmptyList(Section(el2), el3 :: el4_))
          case (el2: Operator) :: el3 :: el4_ =>
            val expr = InfixApp(el3, 0, el2, 0, el1)
            NonEmptyList(expr, el4_)
          case (el2: Operator) :: Nil =>
            NonEmptyList(SectionLeft(el2, 0, el1), Nil)
          case el2 :: el3_ => NonEmptyList(App(el2, 0, el1), el3_)
        }
    }
  }

  @tailrec
  def combine2(stack: NonEmptyList[AST]): AST = {
    stack.tail match {
      case Nil => stack.head
      case _   => combine2(reduce2(stack))
    }
  }

  def addExpr(seg: NonEmptyList[AST], stack: OpList): AST = {
    //      println(s">> addExpr: ${seg.head}")
    seg.head match {
      case Operator(name) =>
        throw new Error("TODO")
      case el =>
        seg.tail match {
          case Nil =>
            val stack2 = ExprNode(el, stack)
            combine(stack2)

          case t :: ts =>
            val seg2   = NonEmptyList(t, ts)
            val stack2 = ExprNode(el, stack)
            addOp(seg2, stack2)
        }
    }
  }
  //

  def addOp(seg: NonEmptyList[AST], stack: ExprList): AST = {
    //      println(s">> addOp: ${seg.head}")
    // a = foo op = + <$> foo <*> bar
    seg.head match {
      case Operator(name) =>
        val op           = OpDesc(Operator(name), descOf(name))
        val shiftedStack = OpNode(op, stack)

        stack match {
          case Empty =>
            val stack2 = OpNode(op, stack)
            seg.tail match {
              case t :: ts => addExpr(NonEmptyList(t, ts), stack2)
              case Nil     => combine(stack2)
            }
          case ExprNode(expr, Empty) =>
            seg.tail match {
              case Nil => combine(shiftedStack)
              case t :: ts =>
                val seg2 = NonEmptyList(t, ts)
                addExpr(seg2, shiftedStack)

            }
          case stack @ ExprNode(expr, OpNode(op2, tail)) =>
            seg.tail match {
              case t :: ts =>
                if (op.desc.prec > op2.desc.prec) {
                  val seg2 = NonEmptyList(t, ts)
                  addExpr(seg2, shiftedStack)
                } else {
                  val stack2 = reduce(stack)
                  addOp(seg, stack2)
                }
              case Nil =>
                if (op.desc.prec > op2.desc.prec) {
                  combine(shiftedStack)
                } else {
                  val stack2 = reduce(stack)
                  addOp(seg, stack2)
                }

            }

        }
    }
  }

  def reduce(node: ExprNode): ExprNode = node.tail match {
    case Empty => node
    case OpNode(op, exprNode) =>
      exprNode match {
        case Empty => ExprNode(SectionLeft(op.op, 0, node.expr), Empty)
        case ExprNode(expr, tail) =>
          ExprNode(InfixApp(expr, 0, op.op, 0, node.expr), tail)
      }
  }

  def combine(node: OpNode): AST = node.tail match {
    case Empty => Section(node.op.op)
    case ExprNode(expr, tail) =>
      combine(ExprNode(SectionRight(expr, 0, node.op.op), tail))
  }

  @tailrec
  def combine(node: ExprNode): AST = node.tail match {
    case Empty => node.expr
    case _     => combine(reduce(node))
  }

}
