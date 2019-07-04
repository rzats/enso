package org.enso.syntax

//class Parser2 extends Flexer.ParserBase[AST] {
//
//  implicit final def charToExpr(char: Char): Flexer.Pattern =
//    Flexer.Ran(char, char)
//  implicit final def stringToExpr(s: String): Flexer.Pattern =
//    s.tail.foldLeft(char(s.head))(_ >> _)
//
//  class ExtendedChar(_this: Char) {
//    final def ||(that: Char): Flexer.Pattern =
//      Flexer.Or(char(_this), char(that))
//  }
//  implicit final def extendChar(i: Char): ExtendedChar   = new ExtendedChar(i)
//  final def char(c: Char):                Flexer.Pattern = range(c, c)
//  final def range(start: Char, end: Char): Flexer.Pattern =
//    Flexer.Ran(start, end)
//  final def range(start: Int, end: Int): Flexer.Pattern = Flexer.Ran(start, end)
//  val any: Flexer.Pattern  = range(5, Int.MaxValue) // FIXME 5 -> 0
//  val pass: Flexer.Pattern = Flexer.Pass
//  val eof: Flexer.Pattern  = char('\0')
//  val none: Flexer.Pattern = Flexer.None_
//
//  def anyOf(alts: Seq[Flexer.Pattern]): Flexer.Pattern = {
//    alts.fold(none)(_ | _)
////    alts.tail.fold(alts.head)(_ | _)
//  }
//
//  def noneOf(chars: String): Flexer.Pattern = {
//    val pointCodes  = chars.map(_.toInt).sorted
//    val startPoints = 5 +: pointCodes.map(_ + 1) // FIXME 5 -> 0
//    val endPoints   = pointCodes.map(_ - 1) :+ Int.MaxValue
//    val ranges      = startPoints.zip(endPoints)
//    val validRanges = ranges.filter { case (s, e) => e >= s }
//    val patterns    = validRanges.map { case (s, e) => range(s, e) }
//    anyOf(patterns)
//  }
//
//  def not(char: Char): Flexer.Pattern =
//    noneOf(char.toString)
//
//  final def replaceGroupSymbols(
//    s: String,
//    lst: List[Flexer.Group]
//  ): String = {
//    var out = s
//    for ((grp, ix) <- lst.zipWithIndex) {
//      out = out.replaceAll(s"___${ix}___", grp.groupIx.toString)
//    }
//    out
//  }
//
//  ////////////////////////////////////
//
//  var result: AST         = _
//  var astStack: List[AST] = Nil
//
//  final def pushAST(): Unit = logger.trace {
//    astStack +:= result
//    result = null
//  }
//
//  final def popAST(): Unit = logger.trace {
//    result   = astStack.head
//    astStack = astStack.tail
//  }
//
//  ////// Offset Management //////
//
//  var lastOffset: Int = 0
//
//  final def useLastOffset(): Int = logger.trace {
//    val offset = lastOffset
//    lastOffset = 0
//    offset
//  }
//
//  ////// Group Management //////
//
//  var groupOffsetStack: List[Int] = Nil
//
//  final def pushGroupOffset(offset: Int): Unit = logger.trace {
//    groupOffsetStack +:= offset
//  }
//
//  final def popGroupOffset(): Int = logger.trace {
//    val offset = groupOffsetStack.head
//    groupOffsetStack = groupOffsetStack.tail
//    offset
//  }
//
//  final def onGroupBegin(): Unit = logger.trace {
//    pushAST()
//    pushGroupOffset(useLastOffset())
//  }
//
//  final def onGroupEnd(): Unit = logger.trace {
//    val offset  = popGroupOffset()
//    val grouped = AST.Group(offset, result, useLastOffset())
//    popAST()
//    app(grouped)
//  }
//
//  ////// Block Management //////
//
//  class BlockState(
//    var isValid: Boolean,
//    var indent: Int,
//    var emptyLines: List[Int],
//    var firstLine: AST,
//    var lines: List[AST.Line]
//  )
//  var blockStack: List[BlockState] = Nil
//  var emptyLines: List[Int]        = Nil
//  var currentBlock: BlockState     = new BlockState(true, 0, Nil, null, Nil)
//
//  final def pushBlock(newIndent: Int): Unit = logger.trace {
//    blockStack +:= currentBlock
//    currentBlock =
//      new BlockState(true, newIndent, emptyLines.reverse, null, Nil)
//    emptyLines = Nil
//  }
//
//  final def popBlock(): Unit = logger.trace {
//    currentBlock = blockStack.head
//    blockStack   = blockStack.tail
//  }
//
//  final def buildBlock(): AST.Block = logger.trace {
//    submitLine()
//    AST.block(
//      currentBlock.indent,
//      currentBlock.emptyLines.reverse,
//      currentBlock.firstLine,
//      currentBlock.lines.reverse
//    )
//  }
//
//  final def submitBlock(): Unit = logger.trace {
//    val block = buildBlock()
//    val block2 =
//      if (currentBlock.isValid) block
//      else AST.Invalid(AST.InvalidBlock(block))
//
//    popAST()
//    popBlock()
//    app(block2)
//    logger.endGroup()
//  }
//
//  final def submitModule(): Unit = logger.trace {
//    val block  = buildBlock()
//    val module = AST.Module(block)
//    result = module
//    logger.endGroup()
//  }
//
//  final def submitLine(): Unit = logger.trace {
//    if (result != null) {
//      if (currentBlock.firstLine == null) {
//        currentBlock.emptyLines = emptyLines
//        currentBlock.firstLine  = result
//      } else {
//        val optResult = Option(result)
//        emptyLines.foreach(currentBlock.lines +:= AST.Line(_, None))
//        currentBlock.lines +:= AST.Line(useLastOffset(), optResult)
//      }
//      emptyLines = Nil
//    }
//  }
//
//  final def onBlockBegin(newIndent: Int): Unit = logger.trace {
//    pushAST()
//    pushBlock(newIndent)
//    logger.beginGroup()
//  }
//
//  final def onBlockNewline(): Unit = logger.trace {
//    submitLine()
//    result = null
//  }
//
//  final def onEmptyLine(): Unit = logger.trace {
//    emptyLines +:= useLastOffset()
//  }
//
//  final def onBlockEnd(newIndent: Int): Unit = logger.trace {
//    while (newIndent < currentBlock.indent) {
//      submitBlock()
//    }
//    if (newIndent > currentBlock.indent) {
//      logger.log("Block with invalid indentation")
//      onBlockBegin(newIndent)
//      currentBlock.isValid = false
//    }
//  }
//
//  ////// Identifiers //////
//
//  var identBody: AST.Identifier = null
//
//  def onIdent(cons: String => AST.Identifier): Unit = logger.trace {
//    onIdent(cons(currentMatch))
//  }
//
//  def onIdent(ast: AST.Identifier): Unit = logger.trace {
//    identBody = ast
//  }
//
//  def submitIdent(): Unit = logger.trace {
//    app(identBody)
//    identBody = null
//  }
//
//  def onIdentErrSfx(): Unit = logger.trace {
//    val ast = AST.InvalidSuffix(identBody, currentMatch)
//    app(ast)
//    identBody = null
//  }
//
//  def finalizeIdent(): Unit = logger.trace {
//    if (identBody != null) submitIdent()
//  }
//
//  ////// Numbers //////
//
//  var numberPart1: String = ""
//  var numberPart2: String = ""
//  var numberPart3: String = ""
//
//  final def numberReset(): Unit = logger.trace {
//    numberPart1 = ""
//    numberPart2 = ""
//    numberPart3 = ""
//  }
//
//  final def submitNumber(): Unit = logger.trace {
//    app(AST.Number(numberPart1, numberPart2, numberPart3))
//  }
//
//  ////// String //////
//
//  def submitEmptyText(): Unit = {
//    app(AST.Text(Vector()))
//  }
//
//  ////// Utils //////
//
//  final def app(fn: String => AST): Unit =
//    app(fn(currentMatch))
//
//  final def app(t: AST): Unit =
//    if (result == null) {
//      result = t
//    } else {
//      result = AST.app(result, useLastOffset(), t)
//    }
//
//  final def onWhitespace(): Unit =
//    onWhitespace(0)
//
//  final def onWhitespace(shift: Int): Unit = logger.trace {
//    val diff = currentMatch.length + shift
//    lastOffset += diff
//    logger.log(s"lastOffset + $diff = $lastOffset")
//  }
//
//  ////// Cleaning //////
//
//  final def onEOF(): Unit = logger.trace {
//    finalizeIdent()
//    onBlockEnd(0)
//    submitModule()
//  }
//
//  final def description(): Unit = {
//
//    ///////////////////////////////////////////
//
//    val lowerLetter = range('a', 'z')
//    val upperLetter = range('A', 'Z')
//    val digit       = range('0', '9')
//    val alphanum    = digit | lowerLetter | upperLetter
//
//    val decimal         = digit.many1
//    val indentChar      = lowerLetter | upperLetter | digit | '_'
//    val identBody       = indentChar.many >> '\''.many
//    val variable        = lowerLetter >> identBody
//    val constructor     = upperLetter >> identBody
//    val whitespace      = ' '.many1
//    val newline         = '\n'
//    val identBreaker    = "^`!@#$%^&*()-=+[]{}|;:<>,./ \t\r\n\\"
//    val operatorChar    = "!$%&*+-/<>?^~|:\\"
//    val operatorErrSfx  = operatorChar | "=" | "," | "."
//    val identErrSfx     = noneOf(identBreaker).many1
//    val specialOperator = "=" | "==" | ">=" | "<=" | "/=" | "." | ".." | "..."
//    val operator        = specialOperator | operatorChar.many1
//
//    val kwDef = "def"
//
//    val NORMAL             = defineGroup("Normal")
//    val IDENT_SFX_CHECK    = defineGroup("Identifier Suffix Check")
//    val OPERATOR_SFX_CHECK = defineGroup("Operator Suffix Check")
//
//    val PARENSED      = defineGroup("Parensed")
//    val NEWLINE       = defineGroup("Newline")
//    val NUMBER_PHASE2 = defineGroup("Number Phase 2")
//    val NUMBER_PHASE3 = defineGroup("Number Phase 3")
//
//    // format: off
//
//    ////// NORMAL //////
//    NORMAL rule whitespace  run Funcx {onWhitespace()}
//    NORMAL rule kwDef       run Funcx {println("def!!!")}
//
//    NORMAL rule newline     run Funcx {beginGroup(NEWLINE)}
//    NORMAL rule "("         run Funcx {onGroupBegin(); beginGroup(PARENSED)}
//    NORMAL rule eof         run Funcx {onEOF()}
//    NORMAL rule operator    run Funcx {app(AST.Operator)}
//    NORMAL rule any         run Funcx {app(AST.Unrecognized)}
//
//
//    ////// IDENTIFIER //////
//    NORMAL rule variable    run Funcx {onIdent(AST.Var); beginGroup(IDENT_SFX_CHECK)}
//    NORMAL rule constructor run Funcx {onIdent(AST.Cons); beginGroup(IDENT_SFX_CHECK)}
//    NORMAL rule "_"         run Funcx {onIdent(AST.Wildcard); beginGroup(IDENT_SFX_CHECK)}
//    IDENT_SFX_CHECK rule identErrSfx run Funcx {onIdentErrSfx();endGroup();}
//    IDENT_SFX_CHECK rule pass        run Funcx {submitIdent();endGroup();}
//
//
//    ////// PARENSED //////
//    PARENSED.cloneRulesFrom(NORMAL)
//    PARENSED rule ")" run Funcx {
//      onGroupEnd()
//      endGroup()
//    }
//
//    // format: on
//
//    ////////////////////////////////
//    // NUMBER (e.g. 16_ff0000.ff) //
//    ////////////////////////////////
//
//    NORMAL rule decimal run Funcx {
//      numberPart2 = currentMatch
//      beginGroup(NUMBER_PHASE2)
//    }
//
//    NUMBER_PHASE2 rule ("_" >> alphanum.many1) run Funcx {
//      endGroup()
//      numberPart1 = numberPart2
//      numberPart2 = currentMatch.substring(1)
//      beginGroup(NUMBER_PHASE3)
//    }
//
//    NUMBER_PHASE2 rule pass run Funcx {
//      endGroup()
//      submitNumber()
//    }
//
//    NUMBER_PHASE3 rule ("." >> alphanum.many1) run Funcx {
//      endGroup()
//      numberPart3 = currentMatch.substring(1)
//      submitNumber()
//    }
//
//    NUMBER_PHASE3 rule pass run Funcx {
//      endGroup()
//      submitNumber()
//    }
//
//    ////////////
//    // String //
//    ////////////
//
//    NORMAL rule "'".many1 run Funcx {
//      val size = currentMatch.length
//      if (size == 2) submitEmptyText()
////      else {
////        pushQuoteSize(size)
////        textBegin()
////        beginGroup(STRING)
////      }
//    }
//
//    ////// NEWLINE //////
//    NEWLINE rule ((whitespace | pass) >> newline) run Funcx {
//      onWhitespace(-1)
//      onEmptyLine()
//    }
//
//    NEWLINE rule (whitespace | pass) run Funcx {
//      onWhitespace()
//      if (lastOffset == currentBlock.indent) {
//        onBlockNewline()
//      } else if (lastOffset > currentBlock.indent) {
//        onBlockBegin(useLastOffset())
//      } else {
//        onBlockEnd(useLastOffset())
//      }
//      endGroup()
//    }
//  }
//
//  final def initialize(): Unit =
//    onBlockBegin(0)
//
//}
