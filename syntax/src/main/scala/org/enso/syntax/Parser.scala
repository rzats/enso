package org.enso.syntax

import org.enso.macros.Func0
import org.enso.syntax.Main.p1
import org.enso.syntax.Flexer

class Parser extends Flexer.ParserBase {
  import org.enso.syntax.AST

  implicit final def charToExpr(char: Char): Flexer.Pattern =
    Flexer.Ran(char, char)
  implicit final def stringToExpr(s: String): Flexer.Pattern =
    s.tail.foldLeft(char(s.head))(_ >> _)

  class ExtendedChar(_this: Char) {
    final def ||(that: Char): Flexer.Pattern =
      Flexer.Or(char(_this), char(that))
  }
  implicit final def extendChar(i: Char): ExtendedChar   = new ExtendedChar(i)
  final def char(c: Char):                Flexer.Pattern = range(c, c)
  final def range(start: Char, end: Char): Flexer.Pattern =
    Flexer.Ran(start, end)
  final def range(start: Int, end: Int): Flexer.Pattern = Flexer.Ran(start, end)
  val any: Flexer.Pattern  = range(0, Int.MaxValue)
  val pass: Flexer.Pattern = Flexer.Pass
  val eof: Flexer.Pattern  = char('\3')

  final def replaceGroupSymbols(
    s: String,
    lst: List[Flexer.Group[Unit]]
  ): String = {
    var out = s
    for ((grp, ix) <- lst.zipWithIndex) {
      out = out.replaceAll(s"___${ix}___", grp.index.toString)
    }
    out
  }

  ////////////////////////////////////

  var result: AST         = null
  var astStack: List[AST] = Nil

  final def pushAST(): Unit = logger.trace {
    astStack +:= result
    result = null
  }

  final def popAST(): Unit = logger.trace {
    result   = astStack.head
    astStack = astStack.tail
  }

  ////// Offset Management //////

  var lastOffset: Int = 0

  final def useLastOffset(): Int = logger.trace {
    val offset = lastOffset
    lastOffset = 0
    offset
  }

  ////// Group Management //////

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
    val grouped = AST.grouped(offset, result, useLastOffset())
    popAST()
    appResult(grouped)
  }

  ////// Indent Management //////

  class BlockState(
    var isValid: Boolean,
    var indent: Int,
    var emptyLines: List[Int],
    var firstLine: AST,
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

  final def submitBlock(): Unit = logger.trace {
    submitLine()
    val block = AST.block(
      currentBlock.indent,
      currentBlock.emptyLines.reverse,
      currentBlock.firstLine,
      currentBlock.lines.reverse,
      currentBlock.isValid
    )
    popAST()
    popBlock()
    appResult(block)
    logger.endGroup()
  }

  final def submitLine(): Unit = logger.trace {
    if (result != null) {
      if (currentBlock.firstLine == null) {
        currentBlock.emptyLines = emptyLines
        currentBlock.firstLine  = result
      } else {
        val optResult = Option(result)
        emptyLines.foreach(currentBlock.lines +:= AST.Spanned(_, None))
        currentBlock.lines +:= AST.Spanned(useLastOffset(), optResult)
      }
      emptyLines = Nil
    }
  }

  final def onBlockBegin(newIndent: Int): Unit = logger.trace {
    pushAST()
    pushBlock(newIndent)
    logger.beginGroup()
  }

  final def onBlockNewline(): Unit = logger.trace {
    submitLine()
    result = null
  }

  final def onEmptyLine(): Unit = logger.trace {
    emptyLines +:= useLastOffset()
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

  ////// Utils //////

  final def ast(sym: String => AST.Symbol): AST =
    ast(sym(currentMatch))

  final def ast(sym: AST.Symbol): AST =
    AST.Spanned(currentMatch.length(), sym)

  final def app(sym: String => AST.Symbol): Unit =
    appResult(ast(sym))

  final def appResult(t: AST): Unit =
    if (result == null) {
      result = t
    } else {
      result = AST.app(result, useLastOffset(), t)
    }

  final def onWhitespace(): Unit =
    onWhitespace(0)

  final def onWhitespace(shift: Int): Unit = logger.trace {
    val diff = currentMatch.length + shift
    lastOffset += diff
    logger.log(s"lastOffset + $diff = $lastOffset")
  }

  ////// Cleaning //////

  final def onEOF(): Unit = logger.trace {
    onBlockEnd(0)
    submitBlock()
  }

  final def description(): Unit = {

    ///////////////////////////////////////////

    val lowerLetter = range('a', 'z')
    val upperLetter = range('A', 'Z')
    val digit       = range('0', '9')
    val indentChar  = lowerLetter | upperLetter | digit | '_'
    val identBody   = indentChar.many >> '\''.many
    val variable    = lowerLetter >> identBody
    val constructor = upperLetter >> identBody
    val whitespace  = ' '.many1
    val newline     = '\n'

    val kwDef  = "def"
    val number = digit.many1

    val NORMAL   = defineGroup[Unit]("Normal")
    val PARENSED = defineGroup[Unit]("Parensed")
    val NEWLINE  = defineGroup[Unit]("Newline")

    // format: off
    
    ////// NORMAL //////
    NORMAL rule whitespace run Func0 {onWhitespace()}
    NORMAL rule kwDef      run Func0 {println("def!!!")}
    NORMAL rule variable   run Func0 {app(AST.Var)}
    NORMAL rule number     run Func0 {}
    NORMAL rule newline    run Func0 {beginGroup(NEWLINE)}
    NORMAL rule "("        run Func0 {beginGroup(PARENSED); onGroupBegin()}
    NORMAL rule eof        run Func0 {onEOF()}

    ////// PARENSED //////
    PARENSED.cloneRulesFrom(NORMAL)
    PARENSED rule ")" run Func0 {
      onGroupEnd()
      endGroup()
    }

    ////// NEWLINE //////
    NEWLINE rule ((whitespace | pass) >> newline) run Func0 {
      onWhitespace(-1)
      onEmptyLine()
    }
    
    NEWLINE rule (whitespace | pass) run Func0 {
      onWhitespace()
      if (lastOffset == currentBlock.indent) {
        onBlockNewline()
      } else if (lastOffset > currentBlock.indent) {
        onBlockBegin(useLastOffset())
      } else {
        onBlockEnd(useLastOffset())
      }
      endGroup()
    }
    // format: on
  }

  final def initialize(): Unit =
    onBlockBegin(0)

}
