package org.enso.syntax.text.test

import org.scalatest._
import org.enso.syntax.Parser
import org.enso.syntax.AST._
import org.enso.syntax.Flexer

class LexerSpec extends FlatSpec with Matchers {

  val parserCls = new Parser().debugCompiledClass()

  def parse(input: String) =
    parserCls.newInstance().run(input)

  def assertModule(input: String, result: AST): Assertion =
    assert(parse(input) == Flexer.Success(result))

  def assertExpr(input: String, result: AST): Assertion = {
    val tt = parse(input)
    tt match {
      case Flexer.Success(value) =>
        assert(value.asInstanceOf[Module].block.firstLine == result)
        assert(value.show() == input)
      case a: Flexer.Partial[_] =>
        fail(s"Parsing failed, consumed ${a.offset} chars")
      case a: Flexer.Failure[_] =>
        fail(s"Parsing failed, consumed ${a.offset} chars")
    }
  }

  def module(input: String, result: AST): Unit =
    it should parseTitle(input) in { assertModule(input, result) }

  def expr(input: String, result: AST): Unit =
    it should parseTitle(input) in { assertExpr(input, result) }

  case object test {
    def expr(input: String): ParseTest = ParseTest(input)
  }

  case class ParseTest(input: String) {

    def as(result: AST): Unit =
      it should parseTitle(input) in { assertExpr(input, result) }
  }

//  def unexpectedSuffix(input: String): Symbol = {
//    Invalid(UnexpectedSuffix(input))
//  }

  def escape(str: String): String =
    str.replace("\n", "\\n")

  def parseTitle(str: String): String =
    "parse " + escape(str)

  // format: off
  /////////////////
  // Identifiers //
  /////////////////


  test expr "_"      as Wildcard
  test expr "Name"   as Cons("Name")
  test expr "name"   as Var("name")
  test expr "name'"  as Var("name'")
  test expr "name''" as Var("name''")
  test expr "name'a" as InvalidSuffix(Var("name'"),"a")
  test expr "name_"  as Var("name_")
  test expr "name_'" as Var("name_'")
  test expr "name'_" as InvalidSuffix(Var("name'"),"_")
  test expr "name`"  as App(5,Var("name"),Unrecognized("`"))

  
  ///////////////
  // Operators //
  ///////////////

  test expr "="   as Operator("=")
  test expr "=="   as Operator("==")
  test expr "==="  as Operator("==") // :: unexpectedSuffix("="))
  test expr ":"    as Operator(":") 
  test expr ","    as Operator(",") 
  test expr "."    as Operator(".") 
  test expr ".."   as Operator("..")
  test expr "..."  as Operator("...")
  test expr "...." as Operator("...")// :: unexpectedSuffix("."))
  test expr ">="   as Operator(">=")
  test expr "<="   as Operator("<=")
  test expr "/="   as Operator("/=")
//  test expr "+="   as Modifier("+=")
//  test expr "-="   as Modifier("-=")
//  test expr "-=-"  as Modifier("-=") // :: unexpectedSuffix("-"))
//
//
//
//  ////////////
//  // Layout //
//  ////////////
//
//  expr(""     , Nil)
//  expr("\n"   , EOL        )
//  expr("\n\n" , EOL :: EOL )
//  expr("\r"   , EOL        )
//  expr("\r\n" , EOL        )
//  expr("\n\r" , EOL         :: EOL)
//  expr("(a)"  , GroupBegin  :: Var("a") :: GroupEnd )
//  expr("[a]"  , ListBegin   :: Var("a") :: ListEnd  )
//  expr("{a}"  , RecordBegin :: Var("a") :: RecordEnd)
//
//
//
//  /////////////
//  // Numbers //
//  /////////////
//
//  expr("7"          , Number(10,7::Nil,Nil))
//  expr("7.5"        , Number(10,7::Nil,Nil) :: Operator(".") :: Number(10,5::Nil,Nil))
//  expr("7_12"       , Number(7,1::2::Nil,Nil))
//  expr("7_12.34"    , Number(7,1::2::Nil,3::4::Nil))
//  expr("16_9acdf"   , Number(16,9::10::12::13::15::Nil,Nil))
//
//
//
//  //////////
//  // Text //
//  //////////
//
//  // Basic
//  expr("'"          , TextBegin   )
//  expr("\""         , TextRawBegin)
//  expr("''"         , TextBegin    :: TextEnd)
//  expr("\"\""       , TextRawBegin :: TextRawEnd)
//  expr("'''"        , TextBegin   )
//  expr("\"\"\""     , TextRawBegin)
//  expr("' '"        , TextBegin    :: Text(" ") :: TextEnd)
//  expr("\" \""      , TextRawBegin :: Text(" ") :: TextRawEnd)
//  expr("'' ''"      , TextBegin    :: TextEnd    :: TextBegin    :: TextEnd   )
//  expr("\"\" \"\""  , TextRawBegin :: TextRawEnd :: TextRawBegin :: TextRawEnd)
//  expr("'\n'"       , TextBegin    :: EOL :: TextEnd   )
//  expr("\"\n\""     , TextRawBegin :: EOL :: TextRawEnd)
//  expr("'\\\\'"     , TextBegin    :: TextEscape(SlashEscape) :: TextEnd)
//  expr("\"\\\\\""   , TextRawBegin :: Text("\\") :: TextEscape(RawQuoteEscape))
//  expr("'\\\''"     , TextBegin    :: TextEscape(QuoteEscape) :: TextEnd   )
//  expr("\"\\\'\""   , TextRawBegin :: TextEscape(QuoteEscape) :: TextRawEnd)
//  expr("'\\\"'"     , TextBegin    :: TextEscape(RawQuoteEscape) :: TextEnd   )
//  expr("\"\\\"\""   , TextRawBegin :: TextEscape(RawQuoteEscape) :: TextRawEnd)
//  expr("''' '''"            , TextBegin    :: Text(" ") :: TextEnd   )
//  expr("\"\"\" \"\"\""      , TextRawBegin :: Text(" ") :: TextRawEnd)
//  expr("''' '' '''"         , TextBegin    :: Text(" ") :: Text("''")   :: Text(" ") :: TextEnd   )
//  expr("\"\"\" \"\" \"\"\"" , TextRawBegin :: Text(" ") :: Text("\"\"") :: Text(" ") :: TextRawEnd)
//
//  // Int Escapes
//  expr("'\\12'"     , TextBegin    :: TextEscape(IntEscape(12)) :: TextEnd   )
//  expr("\"\\12\""   , TextRawBegin :: Text("\\") :: Text("12")  :: TextRawEnd)
//
//  // Char Escapes
//  expr("'\\a'"      , TextBegin :: TextEscape(CharEscape(7)) :: TextEnd)
//  expr("'\\b'"      , TextBegin :: TextEscape(CharEscape(8)) :: TextEnd)
//  expr("'\\f'"      , TextBegin :: TextEscape(CharEscape(12)) :: TextEnd)
//  expr("'\\n'"      , TextBegin :: TextEscape(CharEscape(10)) :: TextEnd)
//  expr("'\\r'"      , TextBegin :: TextEscape(CharEscape(13)) :: TextEnd)
//  expr("'\\t'"      , TextBegin :: TextEscape(CharEscape(9)) :: TextEnd)
//  expr("'\\v'"      , TextBegin :: TextEscape(CharEscape(11)) :: TextEnd)
//  expr("'\\e'"      , TextBegin :: TextEscape(CharEscape(27)) :: TextEnd)
//  expr("'\\q'"      , TextBegin :: TextEscape(InvalidCharEscape('q')) :: TextEnd)
//  expr("\"\\a\""    , TextRawBegin :: Text("\\") :: Text("a") :: TextRawEnd)
//
//  // Control Escapes
//  expr("'\\NUL'"    , TextBegin :: TextEscape(CtrlEscape(0)) :: TextEnd)
//  expr("'\\SOH'"    , TextBegin :: TextEscape(CtrlEscape(1)) :: TextEnd)
//  expr("'\\STX'"    , TextBegin :: TextEscape(CtrlEscape(2)) :: TextEnd)
//  expr("'\\ETX'"    , TextBegin :: TextEscape(CtrlEscape(3)) :: TextEnd)
//  expr("'\\EOT'"    , TextBegin :: TextEscape(CtrlEscape(4)) :: TextEnd)
//  expr("'\\ENQ'"    , TextBegin :: TextEscape(CtrlEscape(5)) :: TextEnd)
//  expr("'\\ACK'"    , TextBegin :: TextEscape(CtrlEscape(6)) :: TextEnd)
//  expr("'\\BEL'"    , TextBegin :: TextEscape(CtrlEscape(7)) :: TextEnd)
//  expr("'\\BS'"     , TextBegin :: TextEscape(CtrlEscape(8)) :: TextEnd)
//  expr("'\\TAB'"    , TextBegin :: TextEscape(CtrlEscape(9)) :: TextEnd)
//  expr("'\\LF'"     , TextBegin :: TextEscape(CtrlEscape(10)) :: TextEnd)
//  expr("'\\VT'"     , TextBegin :: TextEscape(CtrlEscape(11)) :: TextEnd)
//  expr("'\\FF'"     , TextBegin :: TextEscape(CtrlEscape(12)) :: TextEnd)
//  expr("'\\CR'"     , TextBegin :: TextEscape(CtrlEscape(13)) :: TextEnd)
//  expr("'\\SO'"     , TextBegin :: TextEscape(CtrlEscape(14)) :: TextEnd)
//  expr("'\\SI'"     , TextBegin :: TextEscape(CtrlEscape(15)) :: TextEnd)
//  expr("'\\DLE'"    , TextBegin :: TextEscape(CtrlEscape(16)) :: TextEnd)
//  expr("'\\DC1'"    , TextBegin :: TextEscape(CtrlEscape(17)) :: TextEnd)
//  expr("'\\DC2'"    , TextBegin :: TextEscape(CtrlEscape(18)) :: TextEnd)
//  expr("'\\DC3'"    , TextBegin :: TextEscape(CtrlEscape(19)) :: TextEnd)
//  expr("'\\DC4'"    , TextBegin :: TextEscape(CtrlEscape(20)) :: TextEnd)
//  expr("'\\NAK'"    , TextBegin :: TextEscape(CtrlEscape(21)) :: TextEnd)
//  expr("'\\SYN'"    , TextBegin :: TextEscape(CtrlEscape(22)) :: TextEnd)
//  expr("'\\ETB'"    , TextBegin :: TextEscape(CtrlEscape(23)) :: TextEnd)
//  expr("'\\CAN'"    , TextBegin :: TextEscape(CtrlEscape(24)) :: TextEnd)
//  expr("'\\EM'"     , TextBegin :: TextEscape(CtrlEscape(25)) :: TextEnd)
//  expr("'\\SUB'"    , TextBegin :: TextEscape(CtrlEscape(26)) :: TextEnd)
//  expr("'\\ESC'"    , TextBegin :: TextEscape(CtrlEscape(27)) :: TextEnd)
//  expr("'\\FS'"     , TextBegin :: TextEscape(CtrlEscape(28)) :: TextEnd)
//  expr("'\\GS'"     , TextBegin :: TextEscape(CtrlEscape(29)) :: TextEnd)
//  expr("'\\RS'"     , TextBegin :: TextEscape(CtrlEscape(30)) :: TextEnd)
//  expr("'\\US'"     , TextBegin :: TextEscape(CtrlEscape(31)) :: TextEnd)
//  expr("'\\DEL'"    , TextBegin :: TextEscape(CtrlEscape(127)) :: TextEnd)
//  expr("\"\\NUL\""  , TextRawBegin :: Text("\\") :: Text("NUL") :: TextRawEnd)
//
//  // Unicode Escapes
//  expr("'\\uF'"          , TextBegin :: TextEscape(InvalidCharEscape('u')) :: Text("F") :: TextEnd)
//  expr("'\\uFF00'"       , TextBegin :: TextEscape(Uni16Escape(0xFF00)) :: TextEnd)
//  expr("'\\U00ABCDEF'"   , TextBegin :: TextEscape(Uni32Escape(0x00ABCDEF)) :: TextEnd)
//  expr("'\\UFFFFFFFF'"   , TextBegin :: TextEscape(InvalidUni32Escape("FFFFFFFF")) :: TextEnd)
//  expr("'\\u{FF0}'"      , TextBegin :: TextEscape(Uni21Escape(0xFF0)) :: TextEnd)
//  expr("'\\u{FFFFFFFF}'" , TextBegin :: TextEscape(InvalidUni21Escape("FFFFFFFF")) :: TextEnd)
//  expr("'\\u{}'"         , TextBegin :: TextEscape(InvalidUni21Escape("")) :: TextEnd)
//
//  // Interpolation
//  expr("'`{ }`'"        , TextBegin :: TextInterpolateBegin :: RecordBegin :: RecordEnd :: TextInterpolateEnd :: TextEnd)
//  expr("'`{ }'"         , TextBegin :: TextInterpolateBegin :: RecordBegin :: RecordEnd :: TextBegin)
//  expr("'`  `}'"        , TextBegin :: TextInterpolateBegin :: TextInterpolateEnd :: Text("}") :: TextEnd)
//  expr("'`a`'"          , TextBegin :: TextInterpolateBegin :: Var("a") :: TextInterpolateEnd :: TextEnd)
//  expr("'`'a'`'"        , TextBegin :: TextInterpolateBegin :: TextBegin :: Text("a") :: TextEnd :: TextInterpolateEnd :: TextEnd)
//  expr("'''`'a'`'''"    , TextBegin :: TextInterpolateBegin :: TextBegin :: Text("a") :: TextEnd :: TextInterpolateEnd :: TextEnd)
//  expr("'`'''a'''`'"    , TextBegin :: TextInterpolateBegin :: TextBegin :: Text("a") :: TextEnd :: TextInterpolateEnd :: TextEnd)
//  expr("\"``\""         , TextRawBegin :: Text("``") :: TextRawEnd)
//  expr("'`'`a`'`'"      , TextBegin :: TextInterpolateBegin :: TextBegin :: TextInterpolateBegin :: Var("a") :: TextInterpolateEnd :: TextEnd :: TextInterpolateEnd :: TextEnd)
//
//  // Comments
//  expr("#"              , Comment)
//  expr("#c"             , Comment :: CommentBody("c"))
//  expr("#c\na"          , Comment :: CommentBody("c") :: EOL :: Var("a"))
//  expr("#c\n a"         , Comment :: CommentBody("c") :: EOL :: CommentBody(" a"))
//  expr(" #c\n a"        , Comment :: CommentBody("c") :: EOL :: Var("a"))
//  expr(" #c\n  a"       , Comment :: CommentBody("c") :: EOL :: CommentBody("  a"))
//  expr("a#c"            , Var("a") :: Comment :: CommentBody("c"))
//  expr("a # c"          , Var("a") :: Comment :: CommentBody(" c"))
//  expr("a#"             , Var("a") :: Comment)
//  expr("a#\nb"          , Var("a") :: Comment :: EOL :: Var("b"))
//  expr("a#\n b"         , Var("a") :: Comment :: EOL :: CommentBody(" b"))
//
//  // Disabled
//  expr("a #= b"         , Var("a") :: DisabledAssignment :: Var("b"))
//
}
