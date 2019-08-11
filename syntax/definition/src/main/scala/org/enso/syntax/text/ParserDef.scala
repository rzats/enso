package org.enso.syntax.text

import org.enso.flexer._
import org.enso.flexer.automata.Pattern
import org.enso.flexer.automata.Pattern._
import org.enso.syntax.text.AST.Text.Quote
import org.enso.syntax.text.AST.Text.Segment.EOL

import scala.reflect.runtime.universe.reify

case class ParserDef() extends Parser[AST] {

  final def withSome[T, S](opt: Option[T])(f: T => S): S = opt match {
    case None    => throw new Error("Internal Error")
    case Some(a) => f(a)
  }

  /////////////
  //// API ////
  /////////////

  override def run(input: String) = {
    onBlockBegin(0)
    super.run(input)
  }

  ///////////////////////////////////
  //// Basic Char Classification ////
  ///////////////////////////////////

  val lowerLetter: Pattern = range('a', 'z')
  val upperLetter: Pattern = range('A', 'Z')
  val digit: Pattern       = range('0', '9')
  val hex: Pattern         = digit | range('a', 'f') | range('A', 'F')
  val alphaNum: Pattern    = digit | lowerLetter | upperLetter
  val whitespace0: Pattern = ' '.many
  val whitespace: Pattern  = ' '.many1
  val newline: Pattern     = '\n'

  ////////////////
  //// Result ////
  ////////////////

  override def getResult() = result.current

  object result {
    var current: Option[AST]     = None
    var stack: List[Option[AST]] = Nil

    final def push(): Unit = logger.trace {
      logger.log(s"Pushed: $current")
      stack +:= current
      current = None
    }

    final def pop(): Unit = logger.trace {
      current = stack.head
      stack   = stack.tail
      logger.log(s"New result: $current")
    }

    final def app(fn: String => AST): Unit =
      app(fn(currentMatch))

    final def app(t: AST): Unit = logger.trace {
      current = Some(current match {
        case None    => t
        case Some(r) => AST.App(r, off.use(), t)
      })
    }
  }

  ////////////////
  //// Offset ////
  ////////////////

  object off {
    var current: Int     = 0
    var stack: List[Int] = Nil

    final def push(): Unit = logger.trace {
      stack +:= current
      current = 0
    }

    final def pop(): Unit = logger.trace {
      current = stack.head
      stack   = stack.tail
    }

    final def use(): Int = logger.trace {
      val offset = current
      current = 0
      offset
    }

    final def on(): Unit = on(0)
    final def on(shift: Int): Unit = logger.trace {
      val diff = currentMatch.length + shift
      current += diff
      logger.log(s"lastOffset + $diff = $current")
    }
  }

  ////////////////////
  //// IDENTIFIER ////
  ////////////////////

  object ident {
    var current: Option[AST.Ident] = None

    final def on(cons: String => AST.Ident): Unit = logger.trace_ {
      on(cons(currentMatch))
    }

    final def on(ast: AST.Ident): Unit = logger.trace {
      current = Some(ast)
      state.begin(SFX_CHECK)
    }

    final def submit(): Unit = logger.trace {
      withSome(current) { b =>
        result.app(b)
        current = None
      }
    }

    final def onErrSfx(): Unit = logger.trace {
      withSome(current) { b =>
        val ast = AST.Ident.InvalidSuffix(b, currentMatch)
        result.app(ast)
        current = None
        state.end()
      }
    }

    final def onNoErrSfx(): Unit = logger.trace {
      submit()
      state.end()
    }

    final def finalizer(): Unit = logger.trace {
      if (current.isDefined) submit()
    }

    val char: Pattern   = alphaNum | '_'
    val body: Pattern   = char.many >> '\''.many
    val _var: Pattern   = lowerLetter >> body
    val cons: Pattern   = upperLetter >> body
    val breaker: String = "^`!@#$%^&*()-=+[]{}|;:<>,./ \t\r\n\\"
    val errSfx: Pattern = noneOf(breaker).many1

    val SFX_CHECK = state.define("Identifier Suffix Check")
  }

  ROOT            % ident._var   -> reify { ident.on(AST.Var) }
  ROOT            % ident.cons   -> reify { ident.on(AST.Cons) }
  ROOT            % "_"          -> reify { ident.on(AST.Blank) }
  ident.SFX_CHECK % ident.errSfx -> reify { ident.onErrSfx() }
  ident.SFX_CHECK % always       -> reify { ident.onNoErrSfx() }

  //////////////////
  //// Operator ////
  //////////////////

  object opr {}
  final def oprOn(cons: String => AST.Ident): Unit = logger.trace {
    oprOn(cons(currentMatch))
  }

  final def oprOnNoMod(cons: String => AST.Ident): Unit = logger.trace {
    oprOnNoMod(cons(currentMatch))
  }

  final def oprOn(ast: AST.Ident): Unit = logger.trace {
    ident.current = Some(ast)
    state.begin(OPERATOR_MOD_CHECK)
  }

  final def oprOnNoMod(ast: AST.Ident): Unit = logger.trace {
    ident.current = Some(ast)
    state.begin(OPERATOR_SFX_CHECK)
  }

  final def oprOnMod(): Unit = logger.trace {
    withSome(ident.current) { body =>
      ident.current = Some(AST.Opr.Mod(body.asInstanceOf[AST.Opr].name))
    }
  }

  val operatorChar: Pattern    = anyOf("!$%&*+-/<>?^~|:\\")
  val operatorErrChar: Pattern = operatorChar | "=" | "," | "."
  val operatorErrSfx: Pattern  = operatorErrChar.many1
  val eqOperators: Pattern     = "=" | "==" | ">=" | "<=" | "/=" | "#="
  val dotOperators: Pattern    = "." | ".." | "..." | ","
  val operator: Pattern        = operatorChar.many1
  val groupOperators: Pattern  = anyOf("()[]{}")
  val noModOperator: Pattern   = eqOperators | dotOperators | groupOperators

  val OPERATOR_SFX_CHECK = state.define("Operator Suffix Check")
  val OPERATOR_MOD_CHECK = state.define("Operator Modifier Check")
  OPERATOR_MOD_CHECK.parent = OPERATOR_SFX_CHECK

  // format: off
  ROOT             rule operator       run reify { oprOn(AST.Opr(_)) }
  ROOT             rule noModOperator  run reify { oprOnNoMod(AST.Opr(_)) }
  OPERATOR_MOD_CHECK rule "="            run reify { oprOnMod() }
  OPERATOR_SFX_CHECK rule operatorErrSfx run reify { ident.onErrSfx() }
  OPERATOR_SFX_CHECK rule always           run reify { ident.onNoErrSfx() }
  // format: on

  ////////////////////////////////////
  //// NUMBER (e.g. 16_ff0000.ff) ////
  ////////////////////////////////////

  var numberPart1: String = ""
  var numberPart2: String = ""

  final def numberReset(): Unit = logger.trace {
    numberPart1 = ""
    numberPart2 = ""
  }

  final def submitNumber(): Unit = logger.trace {
    val base = if (numberPart1 == "") None else Some(numberPart1)
    result.app(AST.Number(base, numberPart2))
    numberReset()
  }

  final def onDanglingBase(): Unit = logger.trace {
    state.end()
    result.app(AST.Number.DanglingBase(numberPart2))
    numberReset()
  }

  final def onDecimal(): Unit = logger.trace {
    numberPart2 = currentMatch
    state.begin(NUMBER_PHASE2)
  }

  final def onExplicitBase(): Unit = logger.trace {
    state.end()
    numberPart1 = numberPart2
    numberPart2 = currentMatch.substring(1)
    submitNumber()
  }

  final def onNoExplicitBase(): Unit = logger.trace {
    state.end()
    submitNumber()
  }

  val decimal: Pattern = digit.many1

  val NUMBER_PHASE2: State = state.define("Number Phase 2")

  // format: off
  ROOT        rule decimal                 run reify { onDecimal() }
  NUMBER_PHASE2 rule ("_" >> alphaNum.many1) run reify { onExplicitBase() }
  NUMBER_PHASE2 rule ("_")                   run reify { onDanglingBase() }
  NUMBER_PHASE2 rule always                    run reify { onNoExplicitBase() }
  // format: on

  //////////////
  //// Text ////
  //////////////

  var textStateStack: List[AST.Text.Interpolated] = Nil

  final def currentText = textStateStack.head
  final def withCurrentText(f: AST.Text.Interpolated => AST.Text.Interpolated) =
    textStateStack = f(textStateStack.head) :: textStateStack.tail

  final def pushTextState(quoteSize: Quote): Unit = logger.trace {
    textStateStack +:= AST.Text.Interpolated(quoteSize)
  }

  final def popTextState(): Unit = logger.trace {
    textStateStack = textStateStack.tail
  }

  final def insideOfText: Boolean =
    textStateStack.nonEmpty

  final def submitEmptyText(groupIx: State, quoteNum: Quote): Unit =
    logger.trace {
      if (groupIx == RAWTEXT)
        result.app(AST.Text.Raw(quoteNum))
      else
        result.app(AST.Text.Interpolated(quoteNum))
    }

  final def finishCurrentTextBuilding(): AST.Text.Class[_] = logger.trace {
    withCurrentText(t => t.copy(segments = t.segments.reverse))
    val txt =
      if (state.current == RAWTEXT) currentText.raw
      else currentText
    popTextState()
    state.end()
    val singleLine = !txt.segments.contains(EOL())
    if (singleLine || currentBlock.firstLine.isDefined || result.current.isDefined)
      txt
    else {
      val segs =
        AST.Text.MultiLine.stripOffset(currentBlock.indent, txt.segments)
      AST.Text.MultiLine(currentBlock.indent, txt.quoteChar, txt.quote, segs)
    }
  }

  final def submitText(): Unit = logger.trace {
    result.app(finishCurrentTextBuilding())
  }

  final def submitUnclosedText(): Unit = logger.trace {
    result.app(AST.Text.Unclosed(finishCurrentTextBuilding()))
  }

  final def onTextBegin(grp: State, quoteSize: Quote): Unit = logger.trace {
    pushTextState(quoteSize)
    state.begin(grp)
  }

  final def submitPlainTextSegment(
    segment: AST.Text.Interpolated.Segment
  ): Unit =
    logger.trace { withCurrentText(_.prependMergeReversed(segment)) }

  final def submitTextSegment(segment: AST.Text.Interpolated.Segment): Unit =
    logger.trace { withCurrentText(_.prepend(segment)) }

  final def onPlainTextSegment(): Unit = logger.trace {
    submitPlainTextSegment(AST.Text.Segment.Plain(currentMatch))
  }

  final def onTextQuote(quoteSize: Quote): Unit = logger.trace {
    if (currentText.quote == AST.Text.Quote.Triple
        && quoteSize == AST.Text.Quote.Single) onPlainTextSegment()
    else if (currentText.quote == AST.Text.Quote.Single
             && quoteSize == AST.Text.Quote.Triple) {
      val groupIx = state.current
      submitText()
      submitEmptyText(groupIx, AST.Text.Quote.Single)
    } else
      submitText()
  }

  final def onTextEscape(code: AST.Text.Segment.Escape): Unit = logger.trace {
    submitTextSegment(code)
  }

  final def onTextEscapeU16(): Unit = logger.trace {
    val code = currentMatch.drop(2)
    submitTextSegment(AST.Text.Segment.Escape.Unicode.U16(code))
  }

  final def onTextEscapeU32(): Unit = logger.trace {
    val code = currentMatch.drop(2)
    submitTextSegment(AST.Text.Segment.Escape.Unicode.U32(code))
  }

  final def onTextEscapeInt(): Unit = logger.trace {
    val int = currentMatch.drop(1).toInt
    submitTextSegment(AST.Text.Segment.Escape.Number(int))
  }

  final def onInvalidEscape(): Unit = logger.trace {
    val str = currentMatch.drop(1)
    submitTextSegment(AST.Text.Segment.Escape.Invalid(str))
  }

  final def onEscapeSlash(): Unit = logger.trace {
    submitTextSegment(AST.Text.Segment.Escape.Slash)
  }

  final def onEscapeQuote(): Unit = logger.trace {
    submitTextSegment(AST.Text.Segment.Escape.Quote)
  }

  final def onEscapeRawQuote(): Unit = logger.trace {
    submitTextSegment(AST.Text.Segment.Escape.RawQuote)
  }

  final def onInterpolateBegin(): Unit = logger.trace {
    result.push()
    off.push()
    state.begin(INTERPOLATE)
  }

  final def terminateGroupsTill(g: State): Unit = logger.trace {
    while (g != state.current) {
//      getGroup(state.current).finish()
      state.current.finish()
      state.end()
    }
  }

  final def onInterpolateEnd(): Unit = logger.trace {
    if (state.isInside(INTERPOLATE)) {
      terminateGroupsTill(INTERPOLATE)
      submitTextSegment(AST.Text.Segment.Interpolation(result.current))
      result.pop()
      off.pop()
      state.end()
    } else {
      onUnrecognized()
    }
  }

  final def onTextEOF(): Unit = logger.trace {
    submitUnclosedText()
    rewind()
  }

  final def onTextEOL(): Unit = logger.trace {
    submitPlainTextSegment(AST.Text.Segment.EOL())
  }

  val stringChar    = noneOf("'`\"\\\n")
  val stringSegment = stringChar.many1
  val escape_int    = "\\" >> decimal
  val escape_u16    = "\\u" >> repeat(stringChar, 0, 4)
  val escape_u32    = "\\U" >> repeat(stringChar, 0, 8)

  val TEXT: State        = state.define("Text")
  val RAWTEXT: State     = state.define("RawText")
  val INTERPOLATE: State = state.define("Interpolate")
  INTERPOLATE.parent = ROOT

  // format: off
  ROOT  rule '`'            run reify { onInterpolateEnd() }
  TEXT    rule '`'            run reify { onInterpolateBegin() }
  
  ROOT  rule "'"            run reify { onTextBegin(TEXT, AST.Text.Quote.Single) }
  ROOT  rule "'''"          run reify { onTextBegin(TEXT, AST.Text.Quote.Triple) }
  TEXT    rule "'"            run reify { onTextQuote(AST.Text.Quote.Single) }
  TEXT    rule "'''"          run reify { onTextQuote(AST.Text.Quote.Triple) }
  TEXT    rule stringSegment  run reify { onPlainTextSegment() }
  TEXT    rule eof            run reify { onTextEOF() }
  TEXT    rule '\n'           run reify { onTextEOL() }

  ROOT  rule "\""           run reify { onTextBegin(RAWTEXT, AST.Text.Quote.Single) }
  ROOT  rule "\"\"\""       run reify { onTextBegin(RAWTEXT, AST.Text.Quote.Triple) }
  RAWTEXT rule "\""           run reify { onTextQuote(AST.Text.Quote.Single) }
  RAWTEXT rule "\"\"\""       run reify { onTextQuote(AST.Text.Quote.Triple) }
  RAWTEXT rule noneOf("\"\n") run reify { onPlainTextSegment() }
  RAWTEXT rule eof            run reify { onTextEOF() }
  RAWTEXT rule '\n'           run reify { onTextEOL() }

  AST.Text.Segment.Escape.Character.codes.foreach { ctrl =>
    import scala.reflect.runtime.universe._
    val name = TermName(ctrl.toString)
    val func = q"onTextEscape(AST.Text.Segment.Escape.Character.$name)"
    TEXT rule s"\\$ctrl" run func
  }

  AST.Text.Segment.Escape.Control.codes.foreach { ctrl =>
    import scala.reflect.runtime.universe._
    val name = TermName(ctrl.toString)
    val func = q"onTextEscape(AST.Text.Segment.Escape.Control.$name)"
    TEXT rule s"\\$ctrl" run func
  }

  TEXT    rule escape_u16           run reify { onTextEscapeU16() }
  TEXT    rule escape_u32           run reify { onTextEscapeU32() }
  TEXT    rule escape_int           run reify { onTextEscapeInt() }
  TEXT    rule "\\\\"               run reify { onEscapeSlash() }
  TEXT    rule "\\'"                run reify { onEscapeQuote() }
  TEXT    rule "\\\""               run reify { onEscapeRawQuote() }
  TEXT    rule ("\\" >> stringChar) run reify { onInvalidEscape() }
  TEXT    rule "\\"                 run reify { onPlainTextSegment() }

  // format: on

  //////////////
  /// Blocks ///
  //////////////

  class BlockState(
    var isValid: Boolean,
    var indent: Int,
    var emptyLines: List[Int],
    var firstLine: Option[AST.Block.Line.Required],
    var lines: List[AST.Block.Line]
  )
  var blockStack: List[BlockState] = Nil
  var emptyLines: List[Int]        = Nil
  var currentBlock: BlockState     = new BlockState(true, 0, Nil, None, Nil)

  final def pushBlock(newIndent: Int): Unit = logger.trace {
    blockStack +:= currentBlock
    currentBlock =
      new BlockState(true, newIndent, emptyLines.reverse, None, Nil)
    emptyLines = Nil
  }

  final def popBlock(): Unit = logger.trace {
    currentBlock = blockStack.head
    blockStack   = blockStack.tail
  }

  final def buildBlock(): AST.Block = logger.trace {
    submitLine()
    println(result.current)
    withSome(currentBlock.firstLine) { firstLine =>
      AST.Block(
        currentBlock.indent,
        currentBlock.emptyLines.reverse,
        firstLine,
        currentBlock.lines.reverse
      )
    }
  }

  final def submitBlock(): Unit = logger.trace {
    val block = buildBlock()
    val block2 =
      if (currentBlock.isValid) block
      else AST.Block.InvalidIndentation(block)

    result.pop()
    popBlock()
    result.app(block2)
    logger.endGroup()
  }

  final def submitModule(): Unit = logger.trace {
    submitLine()
    val el  = currentBlock.emptyLines.reverse.map(AST.Block.Line(_))
    val el2 = emptyLines.reverse.map(AST.Block.Line(_))
    val firstLines = currentBlock.firstLine match {
      case None            => el
      case Some(firstLine) => firstLine.toOptional :: el
    }
    val lines  = firstLines ++ currentBlock.lines.reverse ++ el2
    val module = AST.Module(lines.head, lines.tail)
    result.current = Some(module)
    logger.endGroup()
  }

  final def submitLine(): Unit = logger.trace {
    result.current match {
      case None => pushEmptyLine()
      case Some(r) =>
        currentBlock.firstLine match {
          case None =>
            val line = AST.Block.Line.Required(r, off.use())
            currentBlock.emptyLines = emptyLines
            currentBlock.firstLine  = Some(line)
          case Some(_) =>
            emptyLines.foreach(currentBlock.lines +:= AST.Block.Line(None, _))
            currentBlock.lines +:= AST.Block
              .Line(result.current, off.use())
        }
        emptyLines = Nil
    }
    result.current = None
  }

  def pushEmptyLine(): Unit = logger.trace {
    emptyLines +:= off.use()
  }

  final def onBlockBegin(newIndent: Int): Unit = logger.trace {
    result.push()
    pushBlock(newIndent)
    logger.beginGroup()
  }

  final def onEmptyLine(): Unit = logger.trace {
    pushEmptyLine()
    off.on(-1)
  }

  final def onEOFLine(): Unit = logger.trace {
    submitLine()
    state.end()
    off.on(-1)
    onEOF()
  }

  final def onNewLine(): Unit = logger.trace {
//    submitLine()
    state.begin(NEWLINE)
  }

  final def onBlockNewline(): Unit = logger.trace {
    state.end()
    off.on()
    if (off.current == currentBlock.indent) {
      submitLine()
    } else if (off.current > currentBlock.indent) {
      onBlockBegin(off.use())
    } else {
      onBlockEnd(off.use())
    }
  }

  final def onBlockEnd(newIndent: Int): Unit = logger.trace {
    while (newIndent < currentBlock.indent) {
      submitBlock()
    }
    if (newIndent > currentBlock.indent) {
      logger.log("Block with invalid indentation")
      onBlockBegin(newIndent)
      currentBlock.isValid = false
    }
  }

  val NEWLINE = state.define("Newline")

  // format: off
  ROOT  rule newline                        run reify { onNewLine() }
  NEWLINE rule ((whitespace|always) >> newline) run reify { onEmptyLine() }
  NEWLINE rule ((whitespace|always) >> eof)     run reify { onEOFLine() }
  NEWLINE rule  (whitespace|always)             run reify { onBlockNewline() }
  // format: on

  ////////////////
  /// Defaults ///
  ////////////////

  final def onUnrecognized(): Unit = logger.trace {
    result.app(AST.Unrecognized)
  }

  final def onEOF(): Unit = logger.trace {
    ident.finalizer()
    onBlockEnd(0)
    submitModule()
  }

  ROOT rule whitespace run reify { off.on() }
  ROOT rule eof run reify { onEOF() }
  ROOT rule any run reify { onUnrecognized() }
}
