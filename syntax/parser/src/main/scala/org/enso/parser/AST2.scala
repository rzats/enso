package org.enso.parser2

/////////
// AST //
/////////

case class AST(offset: Int, span: Int, symbol: AST.Symbol) {
  import AST._

  def show: String = _show(0)

  def _show(indent: Int): String = {
    val strOffset = " " * (indent + offset)
    val strBody = symbol match {
      case Var(name) => name
      case App(f, a) => f.show + a.show
      case Block(body) => {
        "\n" + body.map(_._show(indent + offset)).mkString("\n")
      }
      case Invalid(invalid) =>
        invalid match {
          case InvalidBlock(block) => block._show(indent)
        }
    }
    strOffset + strBody
  }
}

object AST {
  trait Symbol
  case class Var(name: String)              extends Symbol
  case class Cons(name: String)             extends Symbol
  case class Operator(name: String)         extends Symbol
  case class App(func: AST, arg: AST)       extends Symbol
  case class Block(body: Vector[AST])       extends Symbol
  case class Grouped(body: AST)             extends Symbol
  case class Invalid(symbol: InvalidSymbol) extends Symbol

  trait InvalidSymbol
  case class InvalidBlock(block: AST) extends InvalidSymbol

  def app(fn: AST, arg: AST): AST = AST(
    fn.offset,
    fn.span + arg.offset + arg.span,
    App(fn.copy(offset = 0), arg)
  )

  def grouped(begin: Int, body: AST, end: Int): AST = {
    val parenSpan = 1
    val offset    = begin
    val span      = parenSpan + body.offset + body.span + end + parenSpan
    AST(offset, span, Grouped(body))
  }

  def block(offset: Int, lines: List[AST], valid: Boolean): AST = {
    val vlines       = lines.to[Vector].map(_.copy(offset = 0))
    val linesSpan    = lines.foldLeft(0)((i, a) => i + a.span)
    val indentsSpan  = lines.length * offset
    val newlinesSpan = lines.length
    val span         = linesSpan + indentsSpan + newlinesSpan
    if (valid) {
      AST(offset, span, Block(vlines))
    } else {
      AST(offset, span, Invalid(InvalidBlock(AST(0, span, Block(vlines)))))
    }
  }

}
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
