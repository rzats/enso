package org.enso.syntax

object AST {

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
  }

  /////////////////
  ////// AST //////
  /////////////////

  trait AST extends Symbol

  implicit final class _OptionAST_(val self: Option[AST]) extends Symbol {

    override def span: Int =
      self.map(_.span).getOrElse(0)

    override def show(out: CodeBuilder): Unit =
      self.foreach(_.show(out))
  }

  ////// Var //////

  final case object Wildcard extends AST {
    override def span:                   Int  = 1
    override def show(out: CodeBuilder): Unit = out += '_'
  }

  ////// Var //////

  final case class Var(name: String) extends AST {

    override def span: Int =
      name.length

    override def show(out: CodeBuilder): Unit =
      out ++= name
  }

  ////// Cons //////

  final case class Cons(name: String) extends AST {

    override def span: Int =
      name.length

    override def show(out: CodeBuilder): Unit =
      out ++= name
  }

  ////// Operator //////

  final case class Operator(name: String) extends AST {

    override def span: Int =
      name.length

    override def show(out: CodeBuilder): Unit =
      out ++= name
  }

  ////// App //////

  final case class App(span: Int, func: AST, arg: AST) extends AST {

    override def show(out: CodeBuilder): Unit = {
      func.show(out)
      out ++= " " * (span - func.span - arg.span)
      arg.show(out)
    }
  }

  ////// Group //////

  final case class Group(leftOff: Int, body: AST, rightOff: Int) extends AST {

    override def span: Int =
      leftOff + body.span + rightOff

    override def show(out: CodeBuilder): Unit = {
      out ++= " " * leftOff
      body.show(out)
      out ++= " " * rightOff
    }
  }

  ////// Invalid //////

  trait InvalidAST extends AST

  final case class Invalid(symbol: InvalidAST) extends AST {

    override def span: Int =
      symbol.span

    override def show(out: CodeBuilder): Unit =
      symbol.show(out)
  }

  final case class InvalidBlock(block: Block) extends InvalidAST {

    override def span: Int =
      block.span

    override def show(out: CodeBuilder): Unit =
      block.show(out)
  }

  ////// Number //////

  final case class Number(base: String, int: String, frac: String) extends AST {

    override def span: Int =
      base.length + int.length + frac.length

    override def show(out: CodeBuilder): Unit = {
      if (base != "10") {
        out ++= base.toString()
        out += '_'
      }
      out ++= int
      if (frac != "") {
        out += '.'
        out ++= frac
      }
    }

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
    firstLine: AST,
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

  final case class Line(span: Int, elem: Option[AST]) extends Symbol {
    override def show(out: CodeBuilder): Unit = {
      elem.show(out)
      out ++= " " * (span - elem.span)
    }
  }

  ////// Unit //////

  final case class Module(block: Block) extends AST {
    override def span: Int =
      block.span

    override def show(): String =
      block.show().drop(1)
  }

  ////////////////////////////////
  ////// Smart Constructors //////
  ////////////////////////////////

  def app(fn: AST, offset: Int, arg: AST): AST =
    App(fn.span + offset + arg.span, fn, arg)

  def block(
    indent: Int,
    emptyLines: List[Int],
    firstLine: AST,
    lines: List[Line],
    valid: Boolean
  ): AST = {
    val vEmptyLines = emptyLines.to[Vector]
    val vLines      = lines.to[Vector]
    val block       = Block(indent, vEmptyLines, firstLine, vLines)
    if (valid) block else Invalid(InvalidBlock(block))
  }
}
