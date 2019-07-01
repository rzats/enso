package org.enso.syntax

/////////
// AST //
/////////

case class AST(span: Int, symbol: AST.Symbol) {

  import AST._

  def show(): String = {
    val out = new StringBuilder(span)
    show(0, out)
    out.result()
  }

  def show(out: StringBuilder): Unit = show(0, out)

  def show(indent: Int, out: StringBuilder): Unit = {
    out ++= " " * indent
    symbol match {
      case a: Var => out ++= a.name
      case a: App => {
        a.func.show(indent, out)
        out ++= " " * a.offset
        a.arg.show(out)
      }
      case a: Block => {
        val ind = indent + a.indent
        out += '\n'
        a.body.emptyLines.foreach { i =>
          out ++= " " * (ind + i) + "\n"
        }
        a.body.firstLine.show(ind, out)
        a.body.lines.foreach { line =>
          out += '\n'
          line.symbol.foreach(_.show(ind, out))
          out ++= " " * line.offset
        }
      }
    }
  }
}

// format: off
object AST {
  trait Symbol
  case class Var      (name: String)                     extends Symbol
  case class Cons     (name: String)                     extends Symbol
  case class Operator (name: String)                     extends Symbol
  case class App      (func: AST, offset: Int, arg: AST) extends Symbol
  case class Block    (indent: Int, body: BlockBody)     extends Symbol
  case class Grouped  (body: AST)                        extends Symbol
  case class Invalid  (symbol: InvalidSymbol)            extends Symbol

  case class BlockBody
    ( emptyLines : Vector[Int]
    , firstLine  : AST
    , lines      : Vector[Line]
    )

  case class Line (symbol: Option[AST], offset: Int) {
    def span():Int = offset + symbol.map(_.span).getOrElse(0)
  }

  trait InvalidSymbol
  case class InvalidBlock(block: AST) extends InvalidSymbol

  def app(fn: AST, offset:Int, arg: AST): AST =
    AST(fn.span + offset + arg.span, App(fn, offset, arg))

  def grouped(beginOff: Int, body: AST, endOff: Int): AST = {
    val parenSpan = 1
    val span      = parenSpan + beginOff + body.span + endOff + parenSpan
    AST(span, Grouped(body))
  }

  def block
    ( indent     : Int
    , emptyLines : List[Int]
    , firstLine  : AST
    , lines      : List[Line]
    , valid      : Boolean
  ): AST = {
    val vemptyLines    = emptyLines.to[Vector]
    val vlines         = lines.to[Vector]
    val emptyLinesSpan = vemptyLines.sum
    val linesCount     = vlines.length + vemptyLines.length + 1
    val linesSpan      = vlines.foldLeft(firstLine.span)((i, a) => i + a.span)
    val indentsSpan    = linesCount * indent
    val newlinesSpan   = linesCount
    val span           = emptyLinesSpan + linesSpan + indentsSpan + newlinesSpan
    val body           = BlockBody(vemptyLines,firstLine,vlines)
    val block          = AST(span, Block(indent,body))
    if (valid) block else AST(span, Invalid(InvalidBlock(block)))
  }

}
// format: on

// class Sym[T](offset:Int, span:Int, element:T)

////////////
// Symbol //
////////////

//trait Symbol
//
//case object NONE extends Symbol
//
//// Identifiers
//case class Var(name: String)        extends Symbol
//case class Cons(name: String)       extends Symbol
//case class Operator(name: String)   extends Symbol
//case class App(func: AST, arg: AST) extends Symbol
//case class Block(body: Vector[AST]) extends Symbol
//case class Grouped(body: AST)       extends Symbol

//

//object AST {
//
//  def fromToken(tok: Token): AST = {
//    tok.symbol match {
//      case token.Var(name)  => AST(tok.offset, tok.span, Var(name))
//      case token.Cons(name) => AST(tok.offset, tok.span, Cons(name))
//    }
//  }
//
//  def app(fn: AST, arg: AST): AST = {
//    AST(fn.offset, fn.span + arg.span, App(fn.copy(offset = 0), arg))
//  }
//
//  def emptyBlock(): AST = {
//    AST(0, 0, Block(Vector()))
//  }
//
//  def block(lines: Vector[AST]): AST = {
//    AST(0, 0, Block(lines))
//  }
//
//  def grouped(begin: Token, body: AST, end: Token): AST = {
//    val offset = begin.offset
//    val span   = begin.span + body.offset + body.span + end.offset + end.span
//    AST(offset, span, Grouped(body))
//  }
//
//  // FIXME - should report error about lack of closing paren
//  def grouped(begin: Token, body: AST): AST = {
//    val offset = begin.offset
//    val span   = begin.span + body.offset + body.span // + end.offset + end.span
//    AST(offset, span, Grouped(body))
//  }
//
//}
