package org.enso.syntax.text

import org.enso.flexer.Macro
import org.enso.parser.AST._
import org.enso.parser.Parser
import org.enso.{flexer => Flexer}
import org.scalatest._

class LexerSpec extends FlatSpec with Matchers {

  val parserCls = Macro.compile(Parser)

  def parse(input: String) =
    parserCls.run(input) // TODO new isntance?

  def assertModule(input: String, result: AST): Assertion = {
    val tt = parse(input)
    tt match {
      case Flexer.Success(value, offset) =>
        assert(value == result)
        assert(value.show() == input)
      case _ => fail(s"Parsing failed, consumed ${tt.offset} chars")
    }
  }

  def assertExpr(input: String, result: AST): Assertion = {
    val tt = parse(input)
    tt match {
      case Flexer.Success(value, offset) => {
        val module = value.asInstanceOf[Module]
        module.lines match {
          case Nil =>
            module.firstLine.elem match {
              case None => fail("Empty expression")
              case Some(e) => {
                assert(e == result)
                assert(value.show() == input)
              }
            }
          case _ => fail("Multi-line block")
        }
      }
      case _ => fail(s"Parsing failed, consumed ${tt.offset} chars")
    }
  }

  def module(input: String, result: AST): Unit =
    it should parseTitle(input) in { assertModule(input, result) }

  def expr(input: String, result: AST): Unit =
    it should parseTitle(input) in { assertExpr(input, result) }

  case object test {
    def expr(input: String):   ParseTest = ParseTest(input, assertExpr)
    def module(input: String): ParseTest = ParseTest(input, assertModule)
  }

  case class ParseTest(input: String, tester: (String, AST) => Assertion) {

    def as(result: AST): Unit =
      it should parseTitle(input) in { tester(input, result) }
  }

  //  def unexpectedSuffix(input: String): Symbol = {
  //    Invalid(UnexpectedSuffix(input))
  //  }

  def escape(str: String): String =
    str.replace("\n", "\\n")

  def parseTitle(str: String): String = s"parse `${escape(str)}`"

  // format: off

  /////////////////
  // Identifiers //
  /////////////////


  test expr "_"      as Wildcard
  test expr "Name"   as Cons("Name")
  test expr "name"   as "name"
  test expr "name'"  as "name'"
  test expr "name''" as "name''"
  test expr "name'a" as Identifier.InvalidSuffix("name'","a")
  test expr "name_"  as "name_"
  test expr "name_'" as "name_'"
  test expr "name'_" as Identifier.InvalidSuffix("name'","_")
  test expr "name`"  as App("name",0,Unrecognized("`"))


  ///////////////
  // Operators //
  ///////////////

  test expr "++"   as Operator("++")
  test expr "="    as Operator("=")
  test expr "=="   as Operator("==")
  test expr ":"    as Operator(":")
  test expr ","    as Operator(",")
  test expr "."    as Operator(".")
  test expr ".."   as Operator("..")
  test expr "..."  as Operator("...")
  test expr ">="   as Operator(">=")
  test expr "<="   as Operator("<=")
  test expr "/="   as Operator("/=")
  test expr "+="   as Modifier("+")
  test expr "-="   as Modifier("-")
  test expr "==="  as Identifier.InvalidSuffix(Operator("==")  , "=")
  test expr "...." as Identifier.InvalidSuffix(Operator("...") , ".")
  test expr ">=="  as Identifier.InvalidSuffix(Operator(">=")  , "=")
  test expr "+=="  as Identifier.InvalidSuffix(Operator("+")   , "==")


  /////////////////
  // Expressions //
  /////////////////

  test expr "a b"       as ("a" $ "b")
  test expr "a + b"     as ("a" $ Operator("+") $ "b")
  test expr "()"        as Group()
  test expr "(())"      as Group(Group())
  test expr "(()"       as Group.Unclosed(Group())
  test expr "(("        as Group.Unclosed(Group.Unclosed())
  test expr "( "        as Group.Unclosed()
  test expr ")"         as Group.UnmatchedClose
  test expr ")("        as App(Group.UnmatchedClose,0,Group.Unclosed())
  test expr "a ( b c )" as ("a" $ Group(1,"b" $ "c",1))
  test expr "(a (b c))" as Group("a" $ Group("b" $ "c"))

  ////////////
  // Layout //
  ////////////

  test module ""      as Module.oneLiner(Line.empty(0))
  test module "\n"    as Module(Line.empty(0), Line.empty(0) :: Nil)
  test module "  \n " as Module(Line.empty(2), Line.empty(1) :: Nil)
  test module "\n\n"  as Module(Line.empty(0), List.fill(2)(Line.empty(0)))
  //  test module "(a)"  as GroupBegin  :: Var("a") :: GroupEnd
  //  test module "[a]"  as ListBegin   :: Var("a") :: ListEnd
  //  test module "{a}"  as RecordBegin :: Var("a") :: RecordEnd



  /////////////
  // Numbers //
  /////////////

  test expr "7"        as 7
  test expr "07"       as Number("07")
  test expr "10_7"     as Number(10, 7)
  test expr "16_ff"    as Number(16, "ff")
  test expr "16_"      as Number.DanglingBase("16")
  test expr "7.5"      as App(App(7,0,Operator(".")),0,5)
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
