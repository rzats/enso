package org.enso.flexer

import org.enso.Logger

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

trait ParserBase[T] {
  import java.io.{Reader, StringReader}

  import scala.collection.mutable.StringBuilder

  val BUFFERSIZE = 16384

  var sreader: Reader     = null
  val buffer: Array[Char] = new Array(BUFFERSIZE)
  var bufferLen: Int      = 0

  var offset: Int       = -1
  var codePoint: Int    = 0
  val eofChar: Char     = '\0'
  val etxChar: Char     = '\3'
  var currentChar: Char = etxChar

  var matchBuilder = new StringBuilder(64)
  var currentMatch = ""

  val groups: Array[() => Int] = new Array(256)

  val logger = new Logger()

  def getResult(): Option[T]

  def run(input: String): Result[T] = {
    initialize()
    offset    = -1
    codePoint = 0
    sreader   = new StringReader(input)
    val numRead = sreader.read(buffer, 0, buffer.length)
    bufferLen = numRead
    if (numRead == -1) bufferLen = 0
    currentChar = getNextChar
    var r = -1
    while (r == -1) {
      r = step()
    }

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

  def beginGroup(group: Group): Unit =
    beginGroup(group.groupIx)

  def beginGroup(g: Int): Unit = {
    println(s"Begin ${groupLabel(g)}")
    groupStack +:= group
    group = g
  }

  def endGroup(): Unit = {
    val oldGroup = group
    group      = groupStack.head
    groupStack = groupStack.tail
    println(s"End ${groupLabel(oldGroup)}, back to ${groupLabel(group)}")
  }

  def step(): Int = {
    groups(group)()
  }

  def initialize(): Unit

  var groupsx = new ArrayBuffer[Group]()

  def defineGroup(label: String = "unnamed"): Group = {
    val groupIndex = groupsx.length
    val group      = new Group(groupIndex)
    groupsx.append(group)
    groupLabelMap += (groupIndex -> label)
    group
  }

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

  def getNextChar: Char = {
    offset += 1
    val nextChar = if (offset >= bufferLen) {
      if (offset == bufferLen) eofChar
      else etxChar
    } else buffer(offset)
    println(s"Next char '${escapeChar(nextChar)}'")
    nextChar
  }

  final def rewind(): Unit = logger.trace {
    println(s"REWIND ${currentMatch.length}")
    offset -= currentMatch.length + 1
    currentChar = getNextChar
  }

  def specialize(): String = {
    val clsPath = this.getClass.getName.split("\\.")
    val clsName = clsPath.last
    val pkgName = clsPath.dropRight(1).mkString(".")

    val imp  = s"import $pkgName.{$clsName => ParserBase}\n"
    val pfx  = "class GeneratedParser extends ParserBase {\n"
    val sfx  = "}\nscala.reflect.classTag[GeneratedParser].runtimeClass\n"
    val code = imp + pfx + groupsx.map(_.generate()).mkString("\n") + sfx
    code.replace(s"$clsName.", "")
  }

  def debugRun(input: String): Result[T] = {
    val instance = debugInstance()
    instance.run(input)
  }

  def debugInstance(): this.type = {
    val cons = debugCompiledClass()
    cons.newInstance()
  }

  def debugCompiledClass() = {
    println(Console.RED + "Using debug runtime compilation method.")
    println(Console.RED + "Do not use it on production!")
    println(Console.RESET)

    val code     = specialize()
    val toolbox  = currentMirror.mkToolBox()
    val classDef = toolbox.parse(code)

    val clazz = toolbox
      .compile(classDef)
      .apply()
      .asInstanceOf[Class[this.type]]

    clazz.getConstructor()
  }
}
