package org.enso.syntax

object AST {
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

  case class Block(
    emptyLines: Vector[Int],
    firstLine: AST,
    lines: Vector[Line]
  ) extends Symbol {

    def linesCount(): Int =
      emptyLines.length + lines.length + 1

    def linesSpan(): Int =
      emptyLines.sum + lines.foldLeft(firstLine.span)((i, a) => i + a.span)

    def newlinesSpan(): Int =
      linesCount()

    def span(): Int =
      linesSpan() + newlinesSpan()

  }

  type Line = Spanned[Option[AST]]

  trait InvalidSymbol
  case class InvalidBlock(block: AST) extends InvalidSymbol

  implicit class _OptionAST_(val self: Option[AST]) extends AnyVal {
    def span(): Int = self.map(_.span).getOrElse(0)
  }

  /////////////////
  ////// AST //////
  /////////////////

  type AST = Spanned[Symbol]

  implicit class _AST_(val self: AST) {

    def show(): String = {
      val out = new StringBuilder(self.span)
      show(0, out)
      out.result()
    }

    def show(out: StringBuilder): Unit =
      show(0, out)

    def show(indent: Int, out: StringBuilder): Unit = {
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
      }
    }
  }

  ////// Smart Constructors //////

  def app(fn: AST, offset: Int, arg: AST): AST =
    Spanned(fn.span + offset + arg.span, App(fn, arg))

  def grouped(beginOff: Int, body: AST, endOff: Int): AST = {
    val parenSpan = 1
    val span      = parenSpan + beginOff + body.span + endOff + parenSpan
    Spanned(span, Grouped(body))
  }

  def block(
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
