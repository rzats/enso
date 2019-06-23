package org.enso.syntax.text.lexer

import java.io.Reader
import java.io.StringReader

import org.enso.syntax.Main.str
import org.enso.syntax.text.lexer.Lexer
import org.enso.syntax.text.parser.Parser
import org.enso.syntax.text.parser.BParser
import org.enso.syntax.text.parser.Scanner2
import org.enso.syntax.text.lexer.Token
import org.enso.syntax.text.lexer._
import org.enso.syntax.text.parser.AST

import scala.language.implicitConversions
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

class SS(reader: Reader, tokens: Vector[Token]) extends Scanner2(reader) {

//  var ast: AST   = null;
  var astStack: List[AST] = List()

  def pushAST(): Unit = {
    astStack = ast +: astStack
    ast      = null
  }

  def popAST(): Unit = {
    ast      = astStack.head
    astStack = astStack.tail
  }

  def tokenAt(i: Int): Token = tokens(i)

  def astFromChar(i: Int, chr: Char): AST = {
    chr match {
      case 'a' => AST.fromToken(tokenAt(i))
      case 'h' => AST.fromToken(tokenAt(i))
    }
  }

  def astFromStr(i: Int, str: String): AST = {
    val vv = str.to[Vector]
    null
  }

  def appendExprSegment(ast2: AST): Unit = {
    if (ast == null) {
      ast = ast2
    }
    else {
      ast = AST.app(ast, ast2)
    }
  }
}

class SParser(lexreader: Reader) {

  val ss     = new Lexer(lexreader)
  val tokens = ss.lexAll()

  private var _done = false

  val strInput = tokens.map(toChar).mkString("")
  val reader   = new StringReader(strInput)
  val scanner  = new SS(reader, tokens)

  def toChar(tok: Token): Char = {
    tok.symbol match {
      case Var(name)  => 'a'
      case BlockBegin => 'b'
      case BlockEnd   => 'c'
      case EOL        => 'd'
      case EOF        => 'e'
      case GroupBegin => 'f'
      case GroupEnd   => 'g'
      case Cons(name) => 'h'
    }
  }

  def lexAll(): (Vector[AST]) = {
    var builder = Vector.newBuilder[AST]
    do {
      val tok = scanner.yylex
      if (tok == null) {
        _done = true
      }
      builder += tok
    } while (!_done)
    builder.result
  }

  def parse(): AST = {
    lexAll()
    scanner.ast
  }
}

object FunctionWrapper {

  implicit def apply[P, R](fn: P => R): FunctionWrapper[P, R] =
    macro apply_impl[P, R]

  def apply_impl[P: c.WeakTypeTag, R: c.WeakTypeTag](
    c: Context
  )(fn: c.Expr[P => R]
  ): c.Expr[FunctionWrapper[P, R]] = {
    import c.universe._
    c.Expr(q" new FunctionWrapper($fn, ${show(fn.tree)})")
  }
}

class FunctionWrapper[P, R](val fn: P => R, description: String)
    extends Function1[P, R] {
  def apply(p: P)       = fn(p)
  override def toString = description
}
