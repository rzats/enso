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

  final def terminateGroupsTill(g: State): Unit = logger.trace {
    while (g != state.current) {
      state.current.finish()
      state.end()
    }
  }

  /////////////
  //// API ////
  /////////////

  override def run(input: String) = {
    block.onBegin(0)
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
  val space: Pattern       = ' '.many1
  val newline: Pattern     = '\n'

  ////////////////
  //// Result ////
  ////////////////

  override def getResult() = result.current

  object result {
    var current: Option[AST]     = None
    var stack: List[Option[AST]] = Nil

    def push(): Unit = logger.trace {
      logger.log(s"Pushed: $current")
      stack +:= current
      current = None
    }

    def pop(): Unit = logger.trace {
      current = stack.head
      stack   = stack.tail
      logger.log(s"New result: $current")
    }

    def app(fn: String => AST): Unit =
      app(fn(currentMatch))

    def app(t: AST): Unit = logger.trace {
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

    def push(): Unit = logger.trace {
      stack +:= current
      current = 0
    }

    def pop(): Unit = logger.trace {
      current = stack.head
      stack   = stack.tail
    }

    def use(): Int = logger.trace {
      val offset = current
      current = 0
      offset
    }

    def on(): Unit = on(0)

    def on(shift: Int): Unit = logger.trace {
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

    def on(cons: String => AST.Ident): Unit = logger.trace_ {
      on(cons(currentMatch))
    }

    def on(ast: AST.Ident): Unit = logger.trace {
      current = Some(ast)
      state.begin(SFX_CHECK)
    }

    def submit(): Unit = logger.trace {
      withSome(current) { b =>
        result.app(b)
        current = None
      }
    }

    def onErrSfx(): Unit = logger.trace {
      withSome(current) { b =>
        val ast = AST.Ident.InvalidSuffix(b, currentMatch)
        result.app(ast)
        current = None
        state.end()
      }
    }

    def onNoErrSfx(): Unit = logger.trace {
      submit()
      state.end()
    }

    def finalizer(): Unit = logger.trace {
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

  ROOT % ident._var -> reify {
    ident.on(AST.Var)
  }
  ROOT % ident.cons -> reify {
    ident.on(AST.Cons)
  }
  ROOT % "_" -> reify {
    ident.on(AST.Blank)
  }
  ident.SFX_CHECK % ident.errSfx -> reify {
    ident.onErrSfx()
  }
  ident.SFX_CHECK % always -> reify {
    ident.onNoErrSfx()
  }

  //////////////////
  //// Operator ////
  //////////////////

  object opr {
    def on(cons: String => AST.Ident): Unit = logger.trace {
      on(cons(currentMatch))
    }

    def onNoMod(cons: String => AST.Ident): Unit = logger.trace {
      onNoMod(cons(currentMatch))
    }

    def on(ast: AST.Ident): Unit = logger.trace {
      ident.current = Some(ast)
      state.begin(MOD_CHECK)
    }

    def onNoMod(ast: AST.Ident): Unit = logger.trace {
      ident.current = Some(ast)
      state.begin(SFX_CHECK)
    }

    def onMod(): Unit = logger.trace {
      withSome(ident.current) { body =>
        ident.current = Some(AST.Opr.Mod(body.asInstanceOf[AST.Opr].name))
      }
    }

    val char: Pattern     = anyOf("!$%&*+-/<>?^~|:\\")
    val errChar: Pattern  = char | "=" | "," | "."
    val errSfx: Pattern   = errChar.many1
    val body: Pattern     = char.many1
    val opsEq: Pattern    = "=" | "==" | ">=" | "<=" | "/=" | "#="
    val opsDot: Pattern   = "." | ".." | "..." | ","
    val opsGrp: Pattern   = anyOf("()[]{}")
    val opsNoMod: Pattern = opsEq | opsDot | opsGrp

    val SFX_CHECK = state.define("Operator Suffix Check")
    val MOD_CHECK = state.define("Operator Modifier Check")
    MOD_CHECK.parent = SFX_CHECK
  }

  ROOT % opr.body -> reify {
    opr.on(AST.Opr(_))
  }
  ROOT % opr.opsNoMod -> reify {
    opr.onNoMod(AST.Opr(_))
  }
  opr.MOD_CHECK % "=" -> reify {
    opr.onMod()
  }
  opr.SFX_CHECK % opr.errSfx -> reify {
    ident.onErrSfx()
  }
  opr.SFX_CHECK % always -> reify {
    ident.onNoErrSfx()
  }

  ////////////////
  //// NUMBER ////
  ////////////////

  object num {

    var part1: String = ""
    var part2: String = ""

    final def reset(): Unit = logger.trace {
      part1 = ""
      part2 = ""
    }

    final def submit(): Unit = logger.trace {
      val base = if (part1 == "") None else Some(part1)
      result.app(AST.Number(base, part2))
      reset()
    }

    final def onDanglingBase(): Unit = logger.trace {
      state.end()
      result.app(AST.Number.DanglingBase(part2))
      reset()
    }

    final def onDecimal(): Unit = logger.trace {
      part2 = currentMatch
      state.begin(PHASE2)
    }

    final def onExplicitBase(): Unit = logger.trace {
      state.end()
      part1 = part2
      part2 = currentMatch.substring(1)
      submit()
    }

    final def onNoExplicitBase(): Unit = logger.trace {
      state.end()
      submit()
    }

    val decimal: Pattern = digit.many1

    val PHASE2: State = state.define("Number Phase 2")
  }

  ROOT % num.decimal -> reify {
    num.onDecimal()
  }
  num.PHASE2 % ("_" >> alphaNum.many1) -> reify {
    num.onExplicitBase()
  }
  num.PHASE2 % "_" -> reify {
    num.onDanglingBase()
  }
  num.PHASE2 % always -> reify {
    num.onNoExplicitBase()
  }

  //////////////
  //// Text ////
  //////////////

  object text {
    var stack: List[AST.Text.Interpolated] = Nil

    final def current = stack.head

    final def withCurrent(f: AST.Text.Interpolated => AST.Text.Interpolated) =
      stack = f(stack.head) :: stack.tail

    final def push(quoteSize: Quote): Unit = logger.trace {
      stack +:= AST.Text.Interpolated(quoteSize)
    }

    final def pop(): Unit = logger.trace {
      stack = stack.tail
    }

    final def submitEmpty(groupIx: State, quoteNum: Quote): Unit =
      logger.trace {
        if (groupIx == RAW)
          result.app(AST.Text.Raw(quoteNum))
        else
          result.app(AST.Text.Interpolated(quoteNum))
      }

    final def finishCurrent(): AST.Text.Class[_] = logger.trace {
      withCurrent(t => t.copy(segments = t.segments.reverse))
      val txt =
        if (state.current == RAW) current.raw
        else current
      pop()
      state.end()
      val singleLine = !txt.segments.contains(EOL())
      if (singleLine || block.current.firstLine.isDefined || result.current.isDefined)
        txt
      else {
        val segs =
          AST.Text.MultiLine.stripOffset(block.current.indent, txt.segments)
        AST.Text.MultiLine(block.current.indent, txt.quoteChar, txt.quote, segs)
      }
    }

    final def submit(): Unit = logger.trace {
      result.app(finishCurrent())
    }

    final def submit(segment: AST.Text.Interpolated.Segment): Unit =
      logger.trace {
        withCurrent(_.prepend(segment))
      }

    final def submitUnclosed(): Unit = logger.trace {
      result.app(AST.Text.Unclosed(finishCurrent()))
    }

    final def onBegin(grp: State, quoteSize: Quote): Unit = logger.trace {
      push(quoteSize)
      state.begin(grp)
    }

    final def submitPlainSegment(
      segment: AST.Text.Interpolated.Segment
    ): Unit =
      logger.trace {
        withCurrent(_.prependMergeReversed(segment))
      }

    final def onPlainSegment(): Unit = logger.trace {
      submitPlainSegment(AST.Text.Segment.Plain(currentMatch))
    }

    final def onQuote(quoteSize: Quote): Unit = logger.trace {
      if (current.quote == AST.Text.Quote.Triple
          && quoteSize == AST.Text.Quote.Single) onPlainSegment()
      else if (current.quote == AST.Text.Quote.Single
               && quoteSize == AST.Text.Quote.Triple) {
        val groupIx = state.current
        submit()
        submitEmpty(groupIx, AST.Text.Quote.Single)
      } else
        submit()
    }

    final def onEscape(code: AST.Text.Segment.Escape): Unit = logger.trace {
      submit(code)
    }

    final def onEscapeU16(): Unit = logger.trace {
      val code = currentMatch.drop(2)
      submit(AST.Text.Segment.Escape.Unicode.U16(code))
    }

    final def onEscapeU32(): Unit = logger.trace {
      val code = currentMatch.drop(2)
      submit(AST.Text.Segment.Escape.Unicode.U32(code))
    }

    final def onEscapeInt(): Unit = logger.trace {
      val int = currentMatch.drop(1).toInt
      submit(AST.Text.Segment.Escape.Number(int))
    }

    final def onInvalidEscape(): Unit = logger.trace {
      val str = currentMatch.drop(1)
      submit(AST.Text.Segment.Escape.Invalid(str))
    }

    final def onEscapeSlash(): Unit = logger.trace {
      submit(AST.Text.Segment.Escape.Slash)
    }

    final def onEscapeQuote(): Unit = logger.trace {
      submit(AST.Text.Segment.Escape.Quote)
    }

    final def onEscapeRawQuote(): Unit = logger.trace {
      submit(AST.Text.Segment.Escape.RawQuote)
    }

    final def onInterpolateBegin(): Unit = logger.trace {
      result.push()
      off.push()
      state.begin(INTERPOLATE)
    }

    final def onInterpolateEnd(): Unit = logger.trace {
      if (state.isInside(INTERPOLATE)) {
        terminateGroupsTill(INTERPOLATE)
        submit(AST.Text.Segment.Interpolation(result.current))
        result.pop()
        off.pop()
        state.end()
      } else {
        onUnrecognized()
      }
    }

    final def onEOF(): Unit = logger.trace {
      submitUnclosed()
      rewind()
    }

    final def onEOL(): Unit = logger.trace {
      submitPlainSegment(AST.Text.Segment.EOL())
    }

    val stringChar = noneOf("'`\"\\\n")
    val seg        = stringChar.many1
    val escape_int = "\\" >> num.decimal
    val escape_u16 = "\\u" >> repeat(stringChar, 0, 4)
    val escape_u32 = "\\U" >> repeat(stringChar, 0, 8)

    val INTP: State        = state.define("Text")
    val RAW: State         = state.define("RawText")
    val INTERPOLATE: State = state.define("Interpolate")
    INTERPOLATE.parent = ROOT
  }

  ROOT % '`' -> reify {
    text.onInterpolateEnd()
  }
  text.INTP % '`' -> reify {
    text.onInterpolateBegin()
  }

  ROOT % "'" -> reify {
    text.onBegin(text.INTP, AST.Text.Quote.Single)
  }
  ROOT % "'''" -> reify {
    text.onBegin(text.INTP, AST.Text.Quote.Triple)
  }
  text.INTP % "'" -> reify {
    text.onQuote(AST.Text.Quote.Single)
  }
  text.INTP % "'''" -> reify {
    text.onQuote(AST.Text.Quote.Triple)
  }
  text.INTP % text.seg -> reify {
    text.onPlainSegment()
  }
  text.INTP % eof -> reify {
    text.onEOF()
  }
  text.INTP % '\n' -> reify {
    text.onEOL()
  }

  ROOT % "\"" -> reify {
    text.onBegin(text.RAW, AST.Text.Quote.Single)
  }
  ROOT % "\"\"\"" -> reify {
    text.onBegin(text.RAW, AST.Text.Quote.Triple)
  }
  text.RAW % "\"" -> reify {
    text.onQuote(AST.Text.Quote.Single)
  }
  text.RAW % "\"\"\"" -> reify {
    text.onQuote(AST.Text.Quote.Triple)
  }
  text.RAW % noneOf("\"\n") -> reify {
    text.onPlainSegment()
  }
  text.RAW % eof -> reify {
    text.onEOF()
  }
  text.RAW % '\n' -> reify {
    text.onEOL()
  }

  AST.Text.Segment.Escape.Character.codes.foreach { ctrl =>
    import scala.reflect.runtime.universe._
    val name = TermName(ctrl.toString)
    val func = q"text.onEscape(AST.Text.Segment.Escape.Character.$name)"
    text.INTP % s"\\$ctrl" -> func
  }

  AST.Text.Segment.Escape.Control.codes.foreach { ctrl =>
    import scala.reflect.runtime.universe._
    val name = TermName(ctrl.toString)
    val func = q"text.onEscape(AST.Text.Segment.Escape.Control.$name)"
    text.INTP % s"\\$ctrl" -> func
  }

  text.INTP % text.escape_u16 -> reify {
    text.onEscapeU16()
  }
  text.INTP % text.escape_u32 -> reify {
    text.onEscapeU32()
  }
  text.INTP % text.escape_int -> reify {
    text.onEscapeInt()
  }
  text.INTP % "\\\\" -> reify {
    text.onEscapeSlash()
  }
  text.INTP % "\\'" -> reify {
    text.onEscapeQuote()
  }
  text.INTP % "\\\"" -> reify {
    text.onEscapeRawQuote()
  }
  text.INTP % ("\\" >> text.stringChar) -> reify {
    text.onInvalidEscape()
  }
  text.INTP % "\\" -> reify {
    text.onPlainSegment()
  }

  //////////////
  /// Blocks ///
  //////////////

  object block {

    class State(
      var isValid: Boolean,
      var indent: Int,
      var emptyLines: List[Int],
      var firstLine: Option[AST.Block.Line.Required],
      var lines: List[AST.Block.Line]
    )

    var stack: List[State]    = Nil
    var emptyLines: List[Int] = Nil
    var current: State        = new State(true, 0, Nil, None, Nil)

    final def push(newIndent: Int): Unit = logger.trace {
      stack +:= current
      current    = new State(true, newIndent, emptyLines.reverse, None, Nil)
      emptyLines = Nil
    }

    final def pop(): Unit = logger.trace {
      current = stack.head
      stack   = stack.tail
    }

    final def build(): AST.Block = logger.trace {
      submitLine()
      println(result.current)
      withSome(current.firstLine) { firstLine =>
        AST.Block(
          current.indent,
          current.emptyLines.reverse,
          firstLine,
          current.lines.reverse
        )
      }
    }

    final def submit(): Unit = logger.trace {
      val block = build()
      val block2 =
        if (current.isValid) block
        else AST.Block.InvalidIndentation(block)

      result.pop()
      pop()
      result.app(block2)
      logger.endGroup()
    }

    final def submitModule(): Unit = logger.trace {
      submitLine()
      val el  = current.emptyLines.reverse.map(AST.Block.Line(_))
      val el2 = emptyLines.reverse.map(AST.Block.Line(_))
      val firstLines = current.firstLine match {
        case None            => el
        case Some(firstLine) => firstLine.toOptional :: el
      }
      val lines  = firstLines ++ current.lines.reverse ++ el2
      val module = AST.Module(lines.head, lines.tail)
      result.current = Some(module)
      logger.endGroup()
    }

    final def submitLine(): Unit = logger.trace {
      result.current match {
        case None => pushEmptyLine()
        case Some(r) =>
          current.firstLine match {
            case None =>
              val line = AST.Block.Line.Required(r, off.use())
              current.emptyLines = emptyLines
              current.firstLine  = Some(line)
            case Some(_) =>
              emptyLines.foreach(current.lines +:= AST.Block.Line(None, _))
              current.lines +:= AST.Block
                .Line(result.current, off.use())
          }
          emptyLines = Nil
      }
      result.current = None
    }

    def pushEmptyLine(): Unit = logger.trace {
      emptyLines +:= off.use()
    }

    final def onBegin(newIndent: Int): Unit = logger.trace {
      result.push()
      push(newIndent)
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
      state.begin(NEWLINE)
    }

    final def onBlockNewline(): Unit = logger.trace {
      state.end()
      off.on()
      if (off.current == current.indent) {
        submitLine()
      } else if (off.current > current.indent) {
        onBegin(off.use())
      } else {
        onEnd(off.use())
      }
    }

    final def onEnd(newIndent: Int): Unit = logger.trace {
      while (newIndent < current.indent) {
        submit()
      }
      if (newIndent > current.indent) {
        logger.log("Block with invalid indentation")
        onBegin(newIndent)
        current.isValid = false
      }
    }

    val NEWLINE = state.define("Newline")
  }

  ROOT          % newline                -> reify { block.onNewLine() }
  block.NEWLINE % (space.opt >> newline) -> reify { block.onEmptyLine() }
  block.NEWLINE % (space.opt >> eof)     -> reify { block.onEOFLine() }
  block.NEWLINE % space.opt              -> reify { block.onBlockNewline() }

  ////////////////
  /// Defaults ///
  ////////////////

  final def onUnrecognized(): Unit = logger.trace {
    result.app(AST.Unrecognized)
  }

  final def onEOF(): Unit = logger.trace {
    ident.finalizer()
    block.onEnd(0)
    block.submitModule()
  }

  ROOT rule space run reify { off.on() }
  ROOT rule eof run reify { onEOF() }
  ROOT rule any run reify { onUnrecognized() }
}
