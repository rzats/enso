package org.enso.syntax

object AST {

  ///////////////////
  /// Abstraction ///
  ///////////////////

  trait Convertible[-Source, +Target] {
    def convert(source: Source): Target
  }

  trait Showable {
    def show(out: Code): Unit
    def show(): String = (Code() += this).result()
  }

  trait Spanned {
    val span: Int
  }

  ////////////////////
  /// Code Builder ///
  ////////////////////

  final case class Code() {
    type This = this.type

    val stringBuilder: StringBuilder = new StringBuilder()
    var indent: Int                  = 0

    def +=(char: Char):       This = { stringBuilder += char; this }
    def +=(str: String):      This = { stringBuilder ++= str; this }
    def +=(a: Showable):      This = { a.show(this); this }
    def +=(a: Seq[Showable]): This = { a.foreach(this += _); this }
    def +=(off: Int):         This = { stringBuilder ++= " " * off; this }

    def newline(): Unit =
      this += '\n' += indent

    def result(): String =
      stringBuilder.result()

    def withIndent[T](newIndent: Int)(f: => T): T = {
      val oldIndent = indent
      indent = newIndent
      val out = f
      indent = oldIndent
      out
    }

    def withIndentDiff[T](indentDiff: Int): (=> T) => T =
      withIndent(indent + indentDiff)

  }

  //////////////
  /// Symbol ///
  //////////////

  trait Symbol extends Spanned with Showable {
    def to[A](implicit converter: Convertible[this.type, A]): A =
      converter.convert(this)
  }

  ///////////
  /// AST ///
  ///////////

  trait AST extends Symbol {
    def $(arg: AST)    = App(this, 0, arg)
    def $_(arg: AST)   = App(this, 1, arg)
    def $__(arg: AST)  = App(this, 2, arg)
    def $___(arg: AST) = App(this, 3, arg)
  }

  trait InvalidAST extends AST

  implicit final class _OptionAST_(val self: Option[AST]) extends Symbol {
    val span            = self.map(_.span).getOrElse(0)
    def show(out: Code) = self.foreach(out += _)
  }

  /////// Unrecognized //////

  final case class Unrecognized(str: String) extends InvalidAST {
    val span            = str.length
    def show(out: Code) = out += str
  }

  ////// Identifiers //////

  abstract class Identifier(name: String) extends AST {
    val span            = name.length
    def show(out: Code) = out += name
  }

  object Identifier {
    final case class InvalidSuffix(elem: Identifier, suffix: String)
        extends InvalidAST {
      val span            = elem.span + suffix.length
      def show(out: Code) = out += elem += suffix
    }
  }

  final case object Wildcard extends Identifier("_") {
    override val span = 1
  }

  final case class Var(name: String)      extends Identifier(name)
  final case class Cons(name: String)     extends Identifier(name)
  final case class Operator(name: String) extends Identifier(name)
  final case class Modifier(name: String) extends Identifier(name) {
    override val span            = name.length + 1
    override def show(out: Code) = out += name += '='
  }

  implicit def StringToIdentifier(str: String): Identifier = {
    if (str == "") throw new Error("Empty literal")
    if (str == "_") Wildcard
    else if (str.head.isLower) Var(str)
    else if (str.head.isUpper) Cons(str)
    else Operator(str)
  }

  ////// App //////

  final case class App(func: AST, off: Int, arg: AST) extends AST {
    val span            = func.span + off + arg.span
    def show(out: Code) = out += func += off += arg
  }
  object App {
    def apply(func: AST, arg: AST): App = new App(func, 1, arg)
  }

  ////// Group //////

  final case class Group(leftOff: Int, body: Option[AST], rightOff: Int)
      extends AST {
    val span            = leftOff + body.span + rightOff + 2
    def show(out: Code) = out += '(' += leftOff += body += rightOff += ')'
  }

  object Group {
    def apply():                       Group = Group(0, None, 0)
    def apply(l: Int):                 Group = Group(l, None, 0)
    def apply(b: AST):                 Group = Group(0, Some(b), 0)
    def apply(l: Int, b: AST):         Group = Group(l, Some(b), 0)
    def apply(b: AST, r: Int):         Group = Group(0, Some(b), r)
    def apply(l: Int, b: AST, r: Int): Group = Group(l, Some(b), r)

    final case object UnmatchedClose extends InvalidAST {
      val span            = 1
      def show(out: Code) = out += ')'
    }

    final case class Unclosed(leftOff: Int, body: Option[AST])
        extends InvalidAST {
      val span            = leftOff + body.span
      def show(out: Code) = out += '(' += leftOff += body
    }

    object Unclosed {
      def apply():               Unclosed = Unclosed(0, None)
      def apply(b: AST):         Unclosed = Unclosed(0, Some(b))
      def apply(l: Int, b: AST): Unclosed = Unclosed(l, Some(b))
    }

  }

  ////// Number //////

  final case class Number(base: Option[String], int: String) extends AST {
    val span            = base.map(_.length + 1).getOrElse(0) + int.length
    def show(out: Code) = out += base.map(_ + "_").getOrElse("") + int
  }

  object Number {
    def apply(i: Int):               Number = Number(i.toString)
    def apply(i: String):            Number = Number(None, i)
    def apply(b: String, i: String): Number = Number(Some(b), i)
    def apply(b: Int, i: String):    Number = Number(b.toString, i)
    def apply(b: String, i: Int):    Number = Number(b, i.toString)
    def apply(b: Int, i: Int):       Number = Number(b.toString, i.toString)

    final case class DanglingBase(base: String) extends InvalidAST {
      val span            = base.length + 1
      def show(out: Code) = out += base += '_'
    }
  }

  implicit def IntToNumber(int: Int): Number = Number(int)

  ////// Text //////

  final case class Text(quoteNum: Text.QuoteSize, segments: List[Text.Segment])
      extends AST {
    val span            = 2 + segments.map(_.span).sum
    def show(out: Code) = out += '\'' += segments += '\''
  }
  object Text {
    trait QuoteSize
    case object SingleQuote extends QuoteSize
    case object TripleQuote extends QuoteSize

    def apply():             Text = new Text(SingleQuote, Nil)
    def apply(q: QuoteSize): Text = new Text(q, Nil)

    trait Segment extends Symbol
    object Segment {
      final case class Plain(value: String) extends Segment {
        val span            = value.length
        def show(out: Code) = out += value
      }
    }
  }

  ////// Block //////

  final case class Block(
    indent: Int,
    emptyLines: List[Int],
    firstLine: Line.Required,
    lines: List[Line]
  ) extends AST {

    def linesCount: Int =
      emptyLines.length + lines.length + 1

    def linesSpan: Int =
      emptyLines.sum + firstLine.span + lines.map(_.span).sum

    val span: Int = {
      val newlinesSpan = linesCount
      val indentSpan   = linesCount * indent
      linesSpan + newlinesSpan + indentSpan
    }

    def show(out: Code): Unit = {
      val globalIndent = indent + out.indent
      out.withIndent(globalIndent) {
        out += '\n'
        emptyLines.foreach(i => out += globalIndent += i + "\n")
        out += globalIndent
        out += firstLine
        lines.foreach { line =>
          out += '\n'
          out += globalIndent
          out += line
        }
      }
    }
  }

  object Block {
    final case class InvalidIndentation(block: Block) extends InvalidAST {
      val span            = block.span
      def show(out: Code) = block.show(out)
    }
  }

  final case class Line(elem: Option[AST], offset: Int) extends Symbol {
    val span            = elem.span + offset
    def show(out: Code) = out += elem += offset
  }

  object Line {
    val empty              = Line(None, 0)
    def empty(offset: Int) = Line(None, offset)

    final case class Required(elem: AST, offset: Int) extends Symbol {
      val span            = elem.span + offset
      def show(out: Code) = out += elem += offset
    }

    implicit object Required_to_Optional extends Convertible[Required, Line] {
      def convert(src: Required): Line = Line(Some(src.elem), src.offset)
    }
  }

  ////// Unit //////

  final case class Module(firstLine: Line, lines: List[Line]) extends AST {
    val span = firstLine.span + lines.map(_.span).sum
    def show(out: Code) = {
      out += firstLine
      lines.foreach(out += '\n' += _)
    }
  }

  object Module {
    def apply(l: Line):            Module = Module(l, Nil)
    def apply(l: Line, ls: Line*): Module = Module(l, ls.to[List])
  }

}
