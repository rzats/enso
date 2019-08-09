package org.enso.flexer

import org.enso.Logger
import org.enso.flexer.ParserBase._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe.showCode

trait ParserBase[T] {
  import java.io.Reader
  import java.io.StringReader

  import scala.collection.mutable.StringBuilder

  var sreader: Reader     = null
  val buffer: Array[Char] = new Array(BUFFERSIZE)
  var bufferLen: Int      = 1

  var offset: Int          = 0
  var charsToLastRule: Int = 0
  val eofChar: Char        = '\u0000'
  val etxChar: Char        = '\u0003'
  var codePoint: Int       = etxChar.toInt

  var matchBuilder = new StringBuilder(64)
  var currentMatch = ""

  val groups: Array[Int => Int] = new Array(256)

  val logger = new Logger()

  def getResult(): Option[T]

  def run(input: String): Result[T] = {
    initialize()
    sreader = new StringReader(input)
    val numRead = sreader.read(buffer, 1, buffer.length - 1)
    bufferLen = if (numRead == -1) 1 else numRead + 1
    codePoint = getNextCodePoint()

    var r = -1; while (r == -1) r = runGroup()

    getResult() match {
      case None => InternalFailure(offset)
      case Some(result) =>
        if (offset >= bufferLen) Success(result, offset)
        else if (r == -2) Failure(result, offset)
        else Partial(result, offset)
    }
  }

  // Group management

  var groupStack: List[Int]                   = Nil
  val groupLabelMap: mutable.Map[Int, String] = mutable.Map()
  var group: Int                              = 0

  def insideOfGroup(g: Group): Boolean = insideOfGroup(g.groupIx)
  def insideOfGroup(g: Int):   Boolean = group == g || groupStack.contains(g)

  def beginGroup(group: Group): Unit =
    beginGroup(group.groupIx)

  def beginGroup(g: Int): Unit = {
    logger.log(s"Begin ${groupLabel(g)}")
    groupStack +:= group
    group = g
  }

  def endGroup(): Unit = {
    val oldGroup = group
    group      = groupStack.head
    groupStack = groupStack.tail
    logger.log(s"End ${groupLabel(oldGroup)}, back to ${groupLabel(group)}")
  }

  def runGroup(): Int = {
    val nextState  = groups(group)
    var state: Int = 0
    matchBuilder.setLength(0)
    while (state >= 0) {
      logger.log(s"Step: state $group $state, code $codePoint ($currentChar)")
      state = nextState(state)
      if (state >= 0) {
        matchBuilder.append(buffer(offset))
        if (buffer(offset).isHighSurrogate)
          matchBuilder.append(buffer(offset + 1))
        codePoint = getNextCodePoint()
      }
    }
    state
  }

  def initialize(): Unit

  var groupsx = new ArrayBuffer[Group]()

  def defineGroup(label: String = "unnamed", finish: => Unit = {}): Group = {
    val groupIndex = groupsx.length
    val group      = new Group(groupIndex, () => finish)
    groupsx.append(group)
    groupLabelMap += (groupIndex -> label)
    group
  }

  def getGroup(g: Int): Group = groupsx(g)

  def groupLabel(index: Int): String =
    groupLabelMap.get(index) match {
      case None        => "unnamed"
      case Some(label) => label
    }

  def escapeChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _ =>
      if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
      else String.valueOf(ch)
  }
  def getNextCodePoint(): Int = {
    if (offset >= bufferLen)
      return etxChar.toInt
    offset += charSize
    if (offset > BUFFERSIZE - UTFCHARSIZE) {
      val keepChars = Math.max(charsToLastRule, currentMatch.length) + UTFCHARSIZE - 1
      for (i <- 1 to keepChars) buffer(keepChars - i) = buffer(bufferLen - i)
      val numRead = sreader.read(buffer, keepChars, buffer.length - keepChars)
      if (numRead == -1)
        return eofChar.toInt
      offset    = keepChars - (BUFFERSIZE - offset)
      bufferLen = keepChars + numRead
    } else if (offset == bufferLen)
      return eofChar.toInt
    Character.codePointAt(buffer, offset)
  }

  final def rewind(): Unit =
    rewind(currentMatch.length)

  final def rewind(i: Int): Unit = logger.trace {
    offset -= i
    codePoint = getNextCodePoint()
  }

  final def rewindThenCall(rule: () => Unit): Int = {
    rewind(charsToLastRule + 1)
    matchBuilder.setLength(matchBuilder.length - charsToLastRule)
    call(rule)
  }

  final def call(rule: () => Unit): Int = {
    currentMatch    = matchBuilder.result()
    charsToLastRule = 0
    rule()
    -1
  }

  final def currentChar: String = new String(Character.toChars(codePoint))

  final def charSize: Int =
    if (offset >= 0 && buffer(offset).isHighSurrogate) 2 else 1

  def debugGeneratedOutput: String =
    groupsx.map(g => showCode(g.generate())).mkString("\n")
}

object ParserBase {

  val BUFFERSIZE  = 16384
  val UTFCHARSIZE = 2

}
