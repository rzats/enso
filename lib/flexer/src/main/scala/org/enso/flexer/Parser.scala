package org.enso.flexer

import org.enso.Logger
import org.enso.flexer.Parser._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.universe.showCode

trait Parser[T] {
  import Parser._
  import java.io.Reader
  import java.io.StringReader

  import scala.collection.mutable.StringBuilder

  var reader: Reader      = null
  val buffer: Array[Char] = new Array(BUFFER_SIZE)
  var bufferLen: Int      = 0

  val eofChar: Char        = '\u0000'
  val etxChar: Char        = '\u0003'
  var offset: Int          = 0
  var charsToLastRule: Int = 0
  var codePoint: Int       = etxChar.toInt

  var matchBuilder = new StringBuilder(64)
  var currentMatch = ""

  val stateRunners: Array[Int => Int] = new Array(256)

  val logger = new Logger()

  def getResult(): Option[T]

  def run(input: String): Result[T] = {
    initialize()
    reader = new StringReader(input)
    // FIXME: why we have offset 1 here and -1 ?
    val numRead = reader.read(buffer, 1, buffer.length - 1)
    bufferLen = if (numRead == -1) 1 else numRead + 1
    codePoint = getNextCodePoint()

    var runResult = State.EXIT.OK
    while (runResult == State.EXIT.OK) runResult = runCurrentGroup()

    getResult() match {
      case None => InternalFailure(offset)
      case Some(result) =>
        if (offset >= bufferLen) Success(result, offset)
        else if (runResult == State.EXIT.FAIL) Failure(result, offset)
        else Partial(result, offset)
    }
  }

  //// Group management ////

  object _state {
    var stack: List[Int]                   = Nil
    val labelMap: mutable.Map[Int, String] = mutable.Map()
    var current: Int                       = 0

    def insideOf(g: Group): Boolean =
      insideOf(g.groupIx)

    def insideOf(g: Int): Boolean =
      current == g || stack.contains(g)

    def begin(group: Group): Unit =
      begin(group.groupIx)

    def begin(g: Int): Unit = {
      logger.log(s"Begin ${groupLabel(g)}")
      stack +:= current
      current = g
    }

    def end(): Unit = {
      val old = current
      current = stack.head
      stack   = stack.tail
      logger.log(
        s"End ${groupLabel(old)}, back to ${groupLabel(current)}"
      )
    }
  }
  val state = _state
  // FIXME: This is a hack. Without it sbt crashes and needs to be completely
  //        cleaned to compile again.

  def runCurrentGroup(): Int = {
    val grp       = state.current
    val nextState = stateRunners(grp)
    var sss: Int  = State.INITIAL
    matchBuilder.setLength(0)
    while (State.valid(sss)) {
      logger.log(s"Step: state ${grp} $sss, code $codePoint ($currentChar)")
      sss = nextState(sss)
      if (State.valid(sss)) {
        matchBuilder.append(buffer(offset))
        if (buffer(offset).isHighSurrogate)
          matchBuilder.append(buffer(offset + 1))
        codePoint = getNextCodePoint()
      }
    }
    sss
  }

  def initialize(): Unit

  var groupsx = new ArrayBuffer[Group]()

  def defineGroup(label: String = "unnamed", finish: => Unit = {}): Group = {
    val groupIndex = groupsx.length
    val ggggg      = new Group(groupIndex, () => finish)
    groupsx.append(ggggg)
    state.labelMap += (groupIndex -> label)
    ggggg
  }

  def getGroup(g: Int): Group = groupsx(g)

  def groupLabel(index: Int): String =
    state.labelMap.get(index) match {
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
    if (offset > BUFFER_SIZE - UTF_CHAR_SIZE) {
      val keepChars = Math.max(charsToLastRule, currentMatch.length) + UTF_CHAR_SIZE - 1
      for (i <- 1 to keepChars) buffer(keepChars - i) = buffer(bufferLen - i)
      val numRead = reader.read(buffer, keepChars, buffer.length - keepChars)
      if (numRead == -1)
        return eofChar.toInt
      offset    = keepChars - (BUFFER_SIZE - offset)
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

  final def currentChar: String =
    new String(Character.toChars(codePoint))

  final def charSize: Int =
    if (offset >= 0 && buffer(offset).isHighSurrogate) 2 else 1

  def debugGeneratedOutput: String =
    groupsx.map(g => showCode(g.generate())).mkString("\n")
}

object Parser {

  val BUFFER_SIZE   = 16384
  val UTF_CHAR_SIZE = 2

  object State {
    val INITIAL = 0
    object EXIT {
      val OK   = -1
      val FAIL = -2
    }
    def valid(i: Int): Boolean =
      i >= 0
  }
}
