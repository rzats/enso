package org.enso.syntax

object AST {

  trait Convertible[-Source, +Target] {
    def convert(source: Source): Target
  }

  final class CodeBuilder {
    val stringBuilder: StringBuilder = new StringBuilder()
    var indent: Int                  = 0

    def +=(char: Char): Unit =
      stringBuilder += char

    def ++=(str: String): Unit =
      stringBuilder ++= str

    def newline(): Unit = {
      stringBuilder += '\n'
      stringBuilder ++= " " * indent
    }

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

  ////////////////////
  ////// Symbol //////
  ////////////////////

  trait Symbol {
    def span: Int

    def show(out: CodeBuilder): Unit = {
      out ++= show()
    }

    def show(): String = {
      val out = new CodeBuilder
      show(out)
      out.result()
    }

    def to[A](implicit converter: Convertible[this.type, A]): A =
      converter.convert(this)
  }

  /////////////////
  ////// AST //////
  /////////////////

  trait AST        extends Symbol
  trait InvalidAST extends AST

  final case class Invalid(symbol: InvalidAST) extends AST {
    override def span:                   Int  = symbol.span
    override def show(out: CodeBuilder): Unit = symbol.show(out)
  }

  implicit final class _OptionAST_(val self: Option[AST]) extends Symbol {
    override def span:                   Int  = self.map(_.span).getOrElse(0)
    override def show(out: CodeBuilder): Unit = self.foreach(_.show(out))
  }

  /////// Unrecognized //////

  final case class Unrecognized(str: String) extends InvalidAST {
    override def span:                   Int  = str.length
    override def show(out: CodeBuilder): Unit = out ++= str
  }

  ////// Identifiers //////

  trait Identifier extends AST

  final case object Wildcard extends Identifier {
    override def span:                   Int  = 1
    override def show(out: CodeBuilder): Unit = out += '_'
  }

  final case class Var(name: String) extends Identifier {
    override def span:                   Int  = name.length
    override def show(out: CodeBuilder): Unit = out ++= name
  }

  final case class Cons(name: String) extends Identifier {
    override def span:                   Int  = name.length
    override def show(out: CodeBuilder): Unit = out ++= name
  }

  final case class Operator(name: String) extends Identifier {
    override def span:                   Int  = name.length
    override def show(out: CodeBuilder): Unit = out ++= name
  }

  final case class Modifier(name: String) extends Identifier {
    override def span: Int = name.length + 1
    override def show(out: CodeBuilder): Unit = {
      out ++= name
      out += '='
    }
  }

  final case class InvalidSuffix(elem: Identifier, suffix: String)
      extends InvalidAST {
    override def span: Int = elem.span + suffix.length
    override def show(out: CodeBuilder): Unit = {
      elem.show(out)
      out ++= suffix
    }
  }

  ////// Operations //////

  final case class App(span: Int, func: AST, arg: AST) extends AST {
    override def show(out: CodeBuilder): Unit = {
      func.show(out)
      out ++= " " * (span - func.span - arg.span)
      arg.show(out)
    }
  }

  final case class Group(leftOff: Int, body: AST, rightOff: Int) extends AST {
    override def span: Int = leftOff + body.span + rightOff
    override def show(out: CodeBuilder): Unit = {
      out ++= " " * leftOff
      body.show(out)
      out ++= " " * rightOff
    }
  }

  ////// Number //////

  final case class Number(
    base: Option[String],
    int: String,
    frac: Option[String]
  ) extends AST {
    override def span: Int = {
      val baseSpan = base.map(_.length + 1).getOrElse(0)
      val intSpan  = int.length
      val fracSpan = frac.map(_.length + 1).getOrElse(0)
      baseSpan + intSpan + fracSpan
    }
    override def show(out: CodeBuilder): Unit = {
      val pfx = base.map(_ + "_").getOrElse("")
      val sfx = frac.map("." + _).getOrElse("")
      out ++= pfx + int + sfx
    }
  }

  object Number {
    def int(int: String): Number = Number(None, int, None)

    def basedInt(base: String, int: String): Number =
      Number(Some(base), int, None)
  }

  ////// Text //////

  final case class Text(segments: Vector[TextSegment]) extends AST {

    override def span: Int =
      2 + segments.foldLeft(0)((i, s) => i + s.span)

    override def show(out: CodeBuilder): Unit = {
      out += '\''
      segments.foreach(_.show(out))
      out += '\''
    }
  }

  trait TextSegment extends Symbol

  final case class PlainTextSegment(value: String) extends TextSegment {
    override def span: Int =
      value.length

    override def show(out: CodeBuilder): Unit =
      out ++= value
  }

  ////// Block //////

  final case class Block(
    indent: Int,
    emptyLines: Vector[Int],
    firstLine: RequiredLine,
    lines: Vector[Line]
  ) extends AST {

    def linesCount: Int =
      emptyLines.length + lines.length + 1

    def linesSpan: Int =
      emptyLines.sum + lines.foldLeft(firstLine.span)((i, a) => i + a.span)

    override def span: Int = {
      val newlinesSpan = linesCount
      val indentSpan   = linesCount * indent
      linesSpan + newlinesSpan + indentSpan
    }

    override def show(out: CodeBuilder): Unit = {
      val globalIndent    = indent + out.indent
      val globalIndentStr = " " * globalIndent
      out.withIndent(globalIndent) {
        out += '\n'
        emptyLines.foreach(i => out ++= globalIndentStr + " " * i + "\n")
        out ++= globalIndentStr
        firstLine.show(out)
        lines.foreach { line =>
          out += '\n'
          out ++= globalIndentStr
          line.show(out)
        }
      }
    }
  }

  final case class RequiredLine(span: Int, elem: AST) extends Symbol {}

  final case class Line(span: Int, elem: Option[AST]) extends Symbol {
    override def show(out: CodeBuilder): Unit = {
      elem.show(out)
      out ++= " " * (span - elem.span)
    }
  }

  object Line {
    def empty(span: Int) = Line(span, None)
  }

  implicit object RequiredLine_to_Line extends Convertible[RequiredLine, Line] {
    def convert(src: RequiredLine): Line = Line(src.span, Some(src.elem))
  }

  final case class InvalidBlock(block: Block) extends InvalidAST {
    override def span:                   Int  = block.span
    override def show(out: CodeBuilder): Unit = block.show(out)
  }

  ////// Unit //////

  final case class Module(firstLine: Line, lines: List[Line]) extends AST {
    override def span: Int =
      lines.foldLeft(firstLine.span) { case (s, l) => s + l.span }
    override def show(out: CodeBuilder): Unit = {
      firstLine.show(out)
      lines.foreach { line =>
        out += '\n'
        line.show(out)
      }
    }
  }

  object Module {
    def oneLiner(line: Line): Module = Module(line, List())
  }

  ////////////////////////////////
  ////// Smart Constructors //////
  ////////////////////////////////

  def app(fn: AST, offset: Int, arg: AST): AST =
    App(fn.span + offset + arg.span, fn, arg)

  def block(
    indent: Int,
    emptyLines: List[Int],
    firstLine: RequiredLine,
    lines: List[Line]
//    valid: Boolean
  ): Block = {
    val vEmptyLines = emptyLines.to[Vector]
    val vLines      = lines.to[Vector]
    Block(indent, vEmptyLines, firstLine, vLines)
//    val block       = Block(indent, vEmptyLines, firstLine, vLines)
//    if (valid) block else Invalid(InvalidBlock(block))
  }

}
