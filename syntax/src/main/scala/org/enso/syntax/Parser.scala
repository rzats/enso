package org.enso.syntax

import org.enso.macros.Funcx
import org.enso.syntax.Flexer
import org.enso.syntax.Flexer.Pattern
import org.enso.syntax.Flexer.Group

class Parser extends Flexer.ParserBase[AST] {
  import org.enso.syntax.AST

  implicit final def charToExpr(char: Char): Pattern =
    Flexer.Ran(char, char)
  implicit final def stringToExpr(s: String): Pattern =
    s.tail.foldLeft(char(s.head))(_ >> _)

  class ExtendedChar(_this: Char) {
    final def ||(that: Char): Pattern =
      Flexer.Or(char(_this), char(that))
  }
  implicit final def extendChar(i: Char): ExtendedChar = new ExtendedChar(i)
  final def char(c: Char):                Pattern      = range(c, c)
  final def range(start: Char, end: Char): Pattern =
    Flexer.Ran(start, end)
  final def range(start: Int, end: Int): Pattern = Flexer.Ran(start, end)
  val any: Pattern  = range(5, Int.MaxValue) // FIXME 5 -> 0
  val pass: Pattern = Flexer.Pass
  val eof: Pattern  = char('\0')
  val none: Pattern = Flexer.None_

  final def anyOf(chars: String): Pattern =
    anyOf(chars.map(char))

  final def anyOf(alts: Seq[Pattern]): Pattern = {
    alts.fold(none)(_ | _)
//    alts.tail.fold(alts.head)(_ | _)
  }

  final def noneOf(chars: String): Pattern = {
    val pointCodes  = chars.map(_.toInt).sorted
    val startPoints = 5 +: pointCodes.map(_ + 1) // FIXME 5 -> 0
    val endPoints   = pointCodes.map(_ - 1) :+ Int.MaxValue
    val ranges      = startPoints.zip(endPoints)
    val validRanges = ranges.filter { case (s, e) => e >= s }
    val patterns    = validRanges.map { case (s, e) => range(s, e) }
    anyOf(patterns)
  }

  final def not(char: Char): Pattern =
    noneOf(char.toString)

  final def replaceGroupSymbols(
    s: String,
    lst: List[Flexer.Group]
  ): String = {
    var out = s
    for ((grp, ix) <- lst.zipWithIndex) {
      out = out.replaceAll(s"___${ix}___", grp.groupIx.toString)
    }
    out
  }

  ////// Cleaning //////

  final override def initialize(): Unit = {
    onBlockBegin(0)
  }

  //////////////
  /// Result ///
  //////////////

  var result: AST         = _
  var astStack: List[AST] = Nil

  final def pushAST(): Unit = logger.trace {
    astStack +:= result
    result = null
  }

  final def popAST(): Unit = logger.trace {
    result   = astStack.head
    astStack = astStack.tail
  }

  final def app(fn: String => AST): Unit =
    app(fn(currentMatch))

  final def app(t: AST): Unit =
    if (result == null) {
      result = t
    } else {
      result = AST.app(result, useLastOffset(), t)
    }

  /////////////////////////////////
  /// Basic Char Classification ///
  /////////////////////////////////

  val NORMAL = defineGroup("Normal")

  val lowerLetter: Pattern = range('a', 'z')
  val upperLetter: Pattern = range('A', 'Z')
  val digit: Pattern       = range('0', '9')
  val alphaNum: Pattern    = digit | lowerLetter | upperLetter
  val whitespace: Pattern  = ' '.many1
  val newline: Pattern     = '\n'

  //////////////
  /// Offset ///
  //////////////

  var lastOffset: Int = 0

  final def useLastOffset(): Int = logger.trace {
    val offset = lastOffset
    lastOffset = 0
    offset
  }

  final def onWhitespace(): Unit = onWhitespace(0)
  final def onWhitespace(shift: Int): Unit = logger.trace {
    val diff = currentMatch.length + shift
    lastOffset += diff
    logger.log(s"lastOffset + $diff = $lastOffset")
  }

  //////////////////
  /// IDENTIFIER ///
  //////////////////

  var identBody: AST.Identifier = _

  final def onIdent(cons: String => AST.Identifier): Unit = logger.trace {
    onIdent(cons(currentMatch))
  }

  final def onIdent(ast: AST.Identifier): Unit = logger.trace {
    identBody = ast
    beginGroup(IDENT_SFX_CHECK)
  }

  final def submitIdent(): Unit = logger.trace {
    app(identBody)
    identBody = null
  }

  final def onIdentErrSfx(): Unit = logger.trace {
    val ast = AST.InvalidSuffix(identBody, currentMatch)
    app(ast)
    identBody = null
    endGroup()
  }

  final def onNoIdentErrSfx(): Unit = logger.trace {
    submitIdent()
    endGroup();
  }

  final def finalizeIdent(): Unit = logger.trace {
    if (identBody != null) submitIdent()
  }

  val indentChar: Pattern  = alphaNum | '_'
  val identBody_ : Pattern = indentChar.many >> '\''.many
  val variable: Pattern    = lowerLetter >> identBody_
  val constructor: Pattern = upperLetter >> identBody_
  val identBreaker: String = "^`!@#$%^&*()-=+[]{}|;:<>,./ \t\r\n\\"
  val identErrSfx: Pattern = noneOf(identBreaker).many1

  val IDENT_SFX_CHECK = defineGroup("Identifier Suffix Check")

  // format: off
  NORMAL          rule variable    run Funcx { onIdent(AST.Var) }
  NORMAL          rule constructor run Funcx { onIdent(AST.Cons) }
  NORMAL          rule "_"         run Funcx { onIdent(AST.Wildcard) }
  IDENT_SFX_CHECK rule identErrSfx run Funcx { onIdentErrSfx() }
  IDENT_SFX_CHECK rule pass        run Funcx { onNoIdentErrSfx() }
  // format: on

  ////////////////
  /// Operator ///
  ////////////////

  final def onOp(cons: String => AST.Identifier): Unit = logger.trace {
    onOp(cons(currentMatch))
  }

  final def onNoModOp(cons: String => AST.Identifier): Unit = logger.trace {
    onNoModOp(cons(currentMatch))
  }

  final def onOp(ast: AST.Identifier): Unit = logger.trace {
    identBody = ast
    beginGroup(OPERATOR_MOD_CHECK)
  }

  final def onNoModOp(ast: AST.Identifier): Unit = logger.trace {
    identBody = ast
    beginGroup(OPERATOR_SFX_CHECK)
  }

  final def onModifier(): Unit = logger.trace {
    identBody = AST.Modifier(identBody.asInstanceOf[AST.Operator].name)
  }

  val operatorChar: Pattern    = anyOf("!$%&*+-/<>?^~|:\\")
  val operatorErrChar: Pattern = operatorChar | "=" | "," | "."
  val operatorErrSfx: Pattern  = operatorErrChar.many1
  val eqOperators: Pattern     = "=" | "==" | ">=" | "<=" | "/="
  val dotOperators: Pattern    = "." | ".." | "..."
  val operator: Pattern        = operatorChar.many1
  val noModOperator: Pattern   = eqOperators | dotOperators | ","

  val OPERATOR_SFX_CHECK = defineGroup("Operator Suffix Check")
  val OPERATOR_MOD_CHECK = defineGroup("Operator Modifier Check")
  OPERATOR_MOD_CHECK.setParent(OPERATOR_SFX_CHECK)

  // format: off
  NORMAL             rule operator       run Funcx { onOp(AST.Operator) }
  NORMAL             rule noModOperator  run Funcx { onNoModOp(AST.Operator) }
  OPERATOR_MOD_CHECK rule "="            run Funcx { onModifier() }
  OPERATOR_SFX_CHECK rule operatorErrSfx run Funcx { onIdentErrSfx() }
  OPERATOR_SFX_CHECK rule pass           run Funcx { onNoIdentErrSfx() }
  // format: on

  //////////////////////////////////
  /// NUMBER (e.g. 16_ff0000.ff) ///
  //////////////////////////////////

  var numberPart1: String = ""
  var numberPart2: String = ""
  var numberPart3: String = ""

  final def numberReset(): Unit = logger.trace {
    numberPart1 = ""
    numberPart2 = ""
    numberPart3 = ""
  }

  final def submitNumber(): Unit = logger.trace {
    val base = if (numberPart1 == "") None else Some(numberPart1)
    val frac = if (numberPart3 == "") None else Some(numberPart3)
    app(AST.Number(base, numberPart2, frac))
  }

  final def onDecimal(): Unit = logger.trace {
    numberPart2 = currentMatch
    beginGroup(NUMBER_PHASE2)
  }

  final def onExplicitBase(): Unit = logger.trace {
    endGroup()
    numberPart1 = numberPart2
    numberPart2 = currentMatch.substring(1)
    beginGroup(NUMBER_PHASE3)
  }

  final def onNoExplicitBase(): Unit = logger.trace {
    endGroup()
    submitNumber()
  }

  final def onFractional(): Unit = logger.trace {
    endGroup()
    numberPart3 = currentMatch.substring(1)
    submitNumber()
  }

  final def onNoFractional(): Unit = logger.trace {
    endGroup()
    submitNumber()
  }

  val decimal: Pattern = digit.many1

  val NUMBER_PHASE2: Group = defineGroup("Number Phase 2")
  val NUMBER_PHASE3: Group = defineGroup("Number Phase 3")

  // format: off
  NORMAL        rule decimal                 run Funcx { onDecimal() }
  NUMBER_PHASE2 rule ("_" >> alphaNum.many1) run Funcx { onExplicitBase() }
  NUMBER_PHASE2 rule pass                    run Funcx { onNoExplicitBase() }
  NUMBER_PHASE3 rule ("." >> alphaNum.many1) run Funcx { onFractional() }
  NUMBER_PHASE3 rule pass                    run Funcx { onNoFractional() }
  // format: on

  //////////////
  /// String ///
  //////////////

  final def submitEmptyText(): Unit = {
    app(AST.Text(Vector()))
  }

  NORMAL rule "'".many1 run Funcx {
    val size = currentMatch.length
    if (size == 2) submitEmptyText()
//      else {
//        pushQuoteSize(size)
//        textBegin()
//        beginGroup(STRING)
//      }
  }

  //////////////
  /// Groups ///
  //////////////

  var groupOffsetStack: List[Int] = Nil

  final def pushGroupOffset(offset: Int): Unit = logger.trace {
    groupOffsetStack +:= offset
  }

  final def popGroupOffset(): Int = logger.trace {
    val offset = groupOffsetStack.head
    groupOffsetStack = groupOffsetStack.tail
    offset
  }

  final def onGroupBegin(): Unit = logger.trace {
    pushAST()
    pushGroupOffset(useLastOffset())
  }

  final def onGroupEnd(): Unit = logger.trace {
    val offset  = popGroupOffset()
    val grouped = AST.Group(offset, result, useLastOffset())
    popAST()
    app(grouped)
  }

  val PARENSED = defineGroup("Parensed")
  PARENSED.setParent(NORMAL)

  NORMAL rule "(" run Funcx { onGroupBegin(); beginGroup(PARENSED) }
  PARENSED rule ")" run Funcx {
    onGroupEnd()
    endGroup()
  }

  //////////////
  /// Blocks ///
  //////////////

  class BlockState(
    var isValid: Boolean,
    var indent: Int,
    var emptyLines: List[Int],
    var firstLine: AST.RequiredLine,
    var lines: List[AST.Line]
  )
  var blockStack: List[BlockState] = Nil
  var emptyLines: List[Int]        = Nil
  var currentBlock: BlockState     = new BlockState(true, 0, Nil, null, Nil)

  final def pushBlock(newIndent: Int): Unit = logger.trace {
    blockStack +:= currentBlock
    currentBlock =
      new BlockState(true, newIndent, emptyLines.reverse, null, Nil)
    emptyLines = Nil
  }

  final def popBlock(): Unit = logger.trace {
    currentBlock = blockStack.head
    blockStack   = blockStack.tail
  }

  final def buildBlock(): AST.Block = logger.trace {
    submitLine()
    AST.block(
      currentBlock.indent,
      currentBlock.emptyLines.reverse,
      currentBlock.firstLine,
      currentBlock.lines.reverse
    )
  }

  final def submitBlock(): Unit = logger.trace {
    val block = buildBlock()
    val block2 =
      if (currentBlock.isValid) block
      else AST.Invalid(AST.InvalidBlock(block))

    popAST()
    popBlock()
    app(block2)
    logger.endGroup()
  }

  final def submitModule(): Unit = logger.trace {
    submitLine()
    val el  = currentBlock.emptyLines.reverse.map(AST.Line.empty)
    val el2 = emptyLines.reverse.map(AST.Line.empty)
    val firstLines =
      if (currentBlock.firstLine == null) el
      else currentBlock.firstLine.to[AST.Line] :: el
    val lines  = firstLines ++ currentBlock.lines.reverse ++ el2
    val module = AST.Module(lines.head, lines.tail)
    result = module
    logger.endGroup()
  }

  final def submitLine(): Unit = logger.trace {
    if (result != null) {
      if (currentBlock.firstLine == null) {
        currentBlock.emptyLines = emptyLines
        currentBlock.firstLine  = AST.RequiredLine(useLastOffset(), result)
      } else {
        val optResult = Option(result)
        emptyLines.foreach(currentBlock.lines +:= AST.Line(_, None))
        currentBlock.lines +:= AST.Line(useLastOffset(), optResult)
      }
      emptyLines = Nil
    } else {
      pushEmptyLine()
    }
    result = null
  }

  def pushEmptyLine(): Unit = logger.trace {
    emptyLines +:= useLastOffset()
  }

  final def onBlockBegin(newIndent: Int): Unit = logger.trace {
    pushAST()
    pushBlock(newIndent)
    logger.beginGroup()
  }

  final def onEmptyLine(): Unit = logger.trace {
    onWhitespace(-1)
    pushEmptyLine()
  }

  final def onEOFLine(): Unit = logger.trace {
    endGroup()
    onWhitespace(-1)
    onEOF()
  }

  final def onNewLine(): Unit = logger.trace {
    submitLine()
    beginGroup(NEWLINE)
  }

  final def onBlockNewline(): Unit = logger.trace {
    endGroup()
    onWhitespace()
    if (lastOffset == currentBlock.indent) {
      submitLine()
    } else if (lastOffset > currentBlock.indent) {
      onBlockBegin(useLastOffset())
    } else {
      onBlockEnd(useLastOffset())
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

  val NEWLINE = defineGroup("Newline")

  // format: off
  NORMAL  rule newline                        run Funcx { onNewLine() }
  NEWLINE rule ((whitespace|pass) >> newline) run Funcx { onEmptyLine() }
  NEWLINE rule ((whitespace|pass) >> eof)     run Funcx { onEOFLine() }
  NEWLINE rule  (whitespace|pass)             run Funcx { onBlockNewline() }
  // format: on

  ////////////////
  /// Defaults ///
  ////////////////

  final def onEOF(): Unit = logger.trace {
    finalizeIdent()
    onBlockEnd(0)
    submitModule()
  }

  NORMAL rule whitespace run Funcx { onWhitespace() }
  NORMAL rule eof run Funcx { onEOF() }
  NORMAL rule any run Funcx { app(AST.Unrecognized) }

}
