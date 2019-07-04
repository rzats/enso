package org.enso.parser3

object AST {

  trait Showable {
    def show(out: StringBuilder): Unit
  }

  /////////////////////
  ////// Spanned //////
  /////////////////////

  case class Spanned[+T](span: Int, elem: T)

  ////////////////////
  ////// Symbol //////
  ////////////////////

  trait Symbol
  case class Var(name: String)              extends Symbol
  case class Cons(name: String)             extends Symbol
  case class Operator(name: String)         extends Symbol
  case class App(func: AST, arg: AST)       extends Symbol
  case class Grouped(body: AST)             extends Symbol
  case class Invalid(symbol: InvalidSymbol) extends Symbol

  ////// Number //////

  case class Number(base: Int, int: String, frac: String) extends Symbol {
    final def show(): String = {
      val _base = if (base == 10) "" else base.toString() + "_"
      val _frac = if (frac == "") "" else "." + frac
      _base + int + _frac
    }
  }

  object Number {

    final def charToDigit(char: Char): Int = {
      val i = char.toInt
      if (i >= 48 && i <= 57) i - 48 /* 0 to 9 */
      else if (i >= 65 && i <= 90) i - 55 // A to Z
      else if (i >= 97 && i <= 122) i - 87 // a to z
      else -1
    }

    final def stringToDigits(str: String): Vector[Int] = {
      str.to[Vector].map(charToDigit)
    }

    final def fromString(base: String, int: String, frac: String): Number = {
      val base2 = if (base == "") 10 else base.toInt
      Number(base2, int, frac)
    }
  }

  ////// String //////

  case class Text(segments: Vector[TextSegment]) extends Symbol {

    def show(out: StringBuilder): Unit = {
      out += '\''
      segments.foreach(_.show(out))
      out += '\''
    }
  }

  trait TextSegment extends Showable

  case class PlainTextSegment(value: String) extends TextSegment {

    def show(out: StringBuilder): Unit =
      out ++= value
  }

  ////// Block //////

  case class Block(
    emptyLines: Vector[Int],
    firstLine: AST,
    lines: Vector[Line]
  ) extends Symbol {

    final def linesCount(): Int =
      emptyLines.length + lines.length + 1

    final def linesSpan(): Int =
      emptyLines.sum + lines.foldLeft(firstLine.span)((i, a) => i + a.span)

    final def newlinesSpan(): Int =
      linesCount()

    final def span(): Int =
      linesSpan() + newlinesSpan()

  }

  type Line = Spanned[Option[AST]]

  trait InvalidSymbol
  case class InvalidBlock(block: AST) extends InvalidSymbol

  implicit class _OptionAST_(val self: Option[AST]) extends AnyVal {
    final def span(): Int = self.map(_.span).getOrElse(0)
  }

  /////////////////
  ////// AST //////
  /////////////////

  type AST = Spanned[Symbol]

  implicit class _AST_(val self: AST) {

    final def show(): String = {
      val out = new StringBuilder(self.span)
      show(0, out)
      out.result()
    }

    final def show(out: StringBuilder): Unit =
      show(0, out)

    final def show(indent: Int, out: StringBuilder): Unit = {
      val space = "."
      out ++= space * indent
      self.elem match {
        case a: Var => out ++= a.name
        case a: App =>
          a.func.show(indent, out)
          out ++= space * (self.span - a.func.span - a.arg.span)
          a.arg.show(out)
        case a: Block =>
          val ind = indent + (self.span - a.span) / a.linesCount
          out += '\n'
          a.emptyLines.foreach { i =>
            out ++= space * (ind + i) + "\n"
          }
          a.firstLine.show(ind, out)
          a.lines.foreach { line =>
            out += '\n'
            line.elem.foreach(_.show(ind, out))
            out ++= space * (line.span - line.elem.span)
          }
        case a: Number => out ++= a.show()
        case a: Text   => a.show(out)
      }
    }
  }

  ////// Smart Constructors //////

  final def app(fn: AST, offset: Int, arg: AST): AST =
    Spanned(fn.span + offset + arg.span, App(fn, arg))

  final def grouped(beginOff: Int, body: AST, endOff: Int): AST = {
    val parenSpan = 1
    val span      = parenSpan + beginOff + body.span + endOff + parenSpan
    Spanned(span, Grouped(body))
  }

  final def block(
    indent: Int,
    emptyLines: List[Int],
    firstLine: AST,
    lines: List[Line],
    valid: Boolean
  ): AST = {
    val vEmptyLines = emptyLines.to[Vector]
    val vLines      = lines.to[Vector]
    val block       = Block(vEmptyLines, firstLine, vLines)
    val indentsSpan = block.linesCount() * indent
    val span        = indentsSpan + block.span()
    val ast         = Spanned(span, block)
    if (valid) ast else Spanned(span, Invalid(InvalidBlock(ast)))
  }
}
