package org.enso.syntax

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions
import scala.collection.mutable
import scala.collection.immutable
import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox
import org.feijoas.mango.common.{collect => RRR}
import org.feijoas.mango.common.collect.mutable.RangeMap
import java.awt.Desktop
import java.net.URI
import java.net.URLEncoder
import org.enso.macros.Func0
import org.enso.Logger
import scala.annotation.tailrec

object Flexer {

  trait IsoComputeState

  case object NotComputed extends IsoComputeState

  case object InProgress extends IsoComputeState

  case object Computed extends IsoComputeState

  class State {
    //  val links                         = mutable.SortedMap[Int, Int]()
    val isoLinks                      = new mutable.ArrayBuffer[Int]()
    var isos                          = Set[Int]()
    var isosId: Int                   = 0
    var isosComputed: IsoComputeState = NotComputed

    var start  = false
    var end    = false
    var code   = ""
    val links2 = RangeMap[Int, Int, Ordering.Int.type]()
  }

  ////////////////
  // Vocabulary //
  ////////////////

  case class Range(start: Int, end: Int)

  class Vocabulary {
    var divisions = immutable.SortedSet[Int](0, Int.MaxValue)

    def insert(range: Range): Unit = {
      divisions = divisions + range.start
      divisions = divisions + (range.end + 1)
    }

    def size(): Int = divisions.size - 1

    override def toString: String =
      "Vocabulary(" + divisions.toList.map(_.toString).mkString(",") + ")"

    def forEach[U](f: Range => U): Unit = {
      var lastDiv = 0
      for (i <- divisions.drop(1)) {
        f(Range(lastDiv, i - 1))
        lastDiv = i
      }
    }

    def forEachIxed[U](f: ((Range, Int)) => U): Unit = {
      var lastDiv = 0
      for ((i, ix) <- divisions.drop(1).zipWithIndex) {
        f((Range(lastDiv, i - 1), ix))
        lastDiv = i
      }
    }

  }

  /////////
  // NFA //
  /////////

  final class NFA {
    val logger                             = new Logger()
    val states: mutable.ArrayBuffer[State] = new mutable.ArrayBuffer()
    val isoMap: mutable.Map[Set[Int], Int] = mutable.Map()

    val vocabulary = new Vocabulary()

    def addState(): Int = {
      val state = new State()
      states += state
      states.length - 1
    }

    def link(start: Int, end: Int, charStart: Char, charEnd: Char): Unit =
      link(start, end, charStart.toInt, charEnd.toInt)

    def link(start: Int, end: Int, charStart: Int, charEnd: Int): Unit = {
      vocabulary.insert(Range(charStart, charEnd))
      state(start).links2.put(RRR.Range.closed(charStart, charEnd), end)
    }

    def link(start: Int, end: Int, char: Char): Unit =
      link(start, end, char, char)

    def link(start: Int, end: Int): Unit =
      state(start).isoLinks += end

    def visualize(): String = {
      val gray  = "#AAAAAA"
      val lines = mutable.ArrayBuffer[String]()
      lines += "digraph G {"
      lines += "node [shape=circle width=0.8]"
      for ((state, source) <- states.zipWithIndex) {
        if (state.links2.isEmpty) {
          lines += s"""${source} [color="${gray}" fontcolor="${gray}"]"""
        } else {
          lines += s"""${source}"""
        }
        for ((range, target) <- state.links2.asMapOfRanges()) {
          lines += s"""${source} -> ${target} [label="${range}"]"""
        }
        for (target <- state.isoLinks) {
          lines += s"""${source} -> ${target} [style="dashed" color="${gray}"]"""
        }
      }

      lines += "}"
      val code    = lines.mkString("\n")
      var webCode = code
      webCode = URLEncoder.encode(webCode, "UTF-8")
      webCode = webCode.replaceAll("[+]", "%20")
      val address = "https://dreampuf.github.io/GraphvizOnline/#" + webCode
      Desktop.getDesktop().browse(new URI(address))
      code
    }

    def state(ix: Int): State =
      states(ix)

    def computeIsosFor(i: Int): Unit = {
      val s    = state(i)
      var isos = Set[Int](i)
      if (s.isosComputed == NotComputed) {
        var circular = false
        s.isosComputed = InProgress
        s.isoLinks.foreach((tgt) => {
          computeIsosFor(tgt)
          val s2 = state(tgt)
          isos = isos + tgt
          isos = isos ++ s2.isos
          if (s2.isosComputed == InProgress) {
            circular = true
          }
        })
        s.isos = isos
        if (!circular) {
          isoMap.get(isos) match {
            case Some(id) => s.isosId = id
            case None => {
              val id = isoMap.size
              s.isosId = id
              isoMap += (isos -> id)
            }
          }
          s.isosComputed = Computed
        }
      }
    }

    def computeIsos(): Unit =
      for (i <- states.indices) {
        computeIsosFor(i)
      }

    def computeNFAMatrix(): Array[Array[Int]] = {
      logger.group("Computing NFA Matrix") {
        val matrix = Array.ofDim[Int](states.length, vocabulary.size)
        for (stateIx <- states.indices) {
          val s = state(stateIx)
          vocabulary.forEachIxed({
            case (range, vocIx) => {
              s.links2.get(range.start) match {
                case Some(tgt) => matrix(stateIx)(vocIx) = tgt
                case None      => matrix(stateIx)(vocIx) = -1
              }

            }
          })
        }
        matrix
      }
    }

    def computeDFA(): DFA = {
      logger.group("Computing DFA Matrix") {
        val nfaMatrix  = computeNFAMatrix()
        var dfaRows    = 0
        var dfaMatrix  = Array[Array[Int]]()
        val dfaIsoMap  = mutable.Map[Set[Int], Int]()
        val dfaIsoKeys = mutable.ArrayBuffer[Set[Int]]()

        def addDFAKey(key: Set[Int]): Int = {
          val id = dfaIsoMap.size
          dfaIsoMap += (key -> id)
          dfaIsoKeys += key
          dfaRows += 1
          dfaMatrix :+= Array.fill(vocabulary.size) {
            -1
          }
          logger.log(s"DFA[${id}] = ${key}")
          id
        }

        logger.group(s"Preparing start points") {
          val startIsos = state(0).isos
          addDFAKey(startIsos)
        }

        var i = 0
        while (i < dfaRows) {
          val isos = dfaIsoKeys(i)
          logger.group(s"Computing DFA[${i}]") {

            vocabulary.forEachIxed({
              case (voc, vocIx) => {
                logger.group(s"Vocabulary '${voc}'") {
                  var tt = Set[Int]()
                  isos.foreach(iso => {
                    val tgt = nfaMatrix(iso)(vocIx)
                    if (tgt != -1) {
                      tt = tt ++ state(tgt).isos
                    }
                  })
                  if (!tt.isEmpty) {
                    dfaMatrix(i)(vocIx) = dfaIsoMap.get(tt) match {
                      case None => addDFAKey(tt)
                      case Some(id) => {
                        logger.log(s"Existing DFA ID ${id}")
                        id
                      }
                    }
                  }
                }
              }
            })
          }
          i += 1
        }

        val nfaEndStatePriorityMap = mutable.Map[Int, Int]()
        for (i <- nfaMatrix.indices) {
          if (state(i).end) {
            nfaEndStatePriorityMap += (i -> (nfaMatrix.length - i))
          }
        }

        val dfaEndStatePriorityMap = mutable.Map[Int, StateDesc]()
        for ((isos, dfaIx) <- dfaIsoKeys.zipWithIndex) {
          var priority = -1
          var code     = ""
          isos.foreach(iso => {
            nfaEndStatePriorityMap.get(iso) match {
              case None => {}
              case Some(p) => {
                if (p > priority) {
                  priority = p
                  code     = state(iso).code
                }
              }
            }
          })
          if (priority >= 0) {
            dfaEndStatePriorityMap += dfaIx -> StateDesc(priority, code)
          }
        }

        //      println(">>>", nfaEndStatePriorityMap)
        //      println(">>>", dfaEndStatePriorityMap)

        DFA(vocabulary, dfaMatrix, dfaEndStatePriorityMap)
      }
    }
  }

  final case class StateDesc(priority: Int, code: String)

  final case class DFA(
    vocabulary: Vocabulary,
    links: Array[Array[Int]],
    endStatePriorityMap: mutable.Map[Int, StateDesc]
  ) {}

  final case class CodeGen(dfa: DFA) {

    case class Branch(rangeEnd: Int, target: BranchTarget)
    case class BranchTarget(state: Int, code: Option[String])

    var code = new CodeBuilder()

    def cleanBranches(revInput: List[Branch]): List[Branch] = {
      @tailrec def go(rIn: List[Branch], out: List[Branch]): List[Branch] =
        rIn match {
          case Nil => out
          case i :: is =>
            out match {
              case Nil => go(is, i :: Nil)
              case o :: os =>
                if (o.target == i.target) go(is, out)
                else go(is, i :: out)
            }
        }
      go(revInput, Nil)
    }

    def genBranches(branches: List[Branch]): Unit = {
      @tailrec def go(bss: List[Branch], first: Boolean): Unit =
        bss match {
          case Nil => code.add("else -2")
          case b :: bs => {
            if (!first) code.add("else ")
            code._ifLTE("codePoint", b.rangeEnd) {
              var targetStatex = b.target.state
              if (b.target.state == -1) {
                b.target.code match {
                  case None    => targetStatex = -2
                  case Some(c) => code.addLine(c)
                }
              }
              code.add(targetStatex.toString())
            }
            go(bs, first = false)
          }
        }
      go(branches, first = true)
    }

    def generateStateMatch(): Unit = {
      code.add("state = ")
      code._match("state") {
        for (state <- dfa.links.indices) {
          code._case(state) {

            var revBranches: List[Branch] = Nil
            dfa.vocabulary.forEachIxed({
              case (range, vocIx) => {
                val targetState  = dfa.links(state)(vocIx)
                val p1           = dfa.endStatePriorityMap.get(state)
                val branchTarget = BranchTarget(targetState, p1.map(_.code))
                revBranches ::= Branch(range.end, branchTarget)
              }
            })

            val branches = cleanBranches(revBranches)
            genBranches(branches)
          }
        }
      }
    }

    def generate(i: Int): String = {
      code
        .add(s"def runGroup${i}(): Int = ")
        .block {
          code
            .addLine("var state: Int = 0")
            .addLine("matchBuilder.setLength(0)")
            .add("while(state >= 0)")
            .block {
              code.addLine("codePoint = currentChar.toInt")
              generateStateMatch()
              code._if("state >= 0") {
                code.addLine("matchBuilder.append(currentChar)")
                code.addLine("currentChar = getNextChar")
              }
            }
          code.addLine("state")
        }
      code.build()
    }
  }

  class CodeBuilder {
    var lines       = mutable.ArrayBuffer[String]()
    var currentLine = ""
    var indentation = 1

    def submitLine(): CodeBuilder = {
      if (!currentLine.isEmpty) {
        lines += "  " * indentation + currentLine
        currentLine = ""
      }
      this
    }

    def add(s: String): CodeBuilder = {
      currentLine += s
      this
    }

    def addLine(s: String): CodeBuilder =
      add(s).submitLine()

    def comment(s: String): CodeBuilder =
      addLine(s"// ${s}")

    def _match[T](s: Any)(f: => T): CodeBuilder =
      add(s"${s} match").block(f)

    def _case[T](s: Any)(f: => T): CodeBuilder =
      add(s"case ${s} =>").block(f)

    def _if[T](s: String)(f: => T): CodeBuilder =
      add(s"if (${s})").block(f)

    def _else_if[T](s: String)(f: => T): CodeBuilder =
      add("else ")._if(s)(f)

    def _ifLTE[T](left: Any, right: Any)(f: => T): CodeBuilder =
      _if(s"${left} <= ${right}")(f)

    def _else_ifLTE[T](left: Any, right: Any)(f: => T): CodeBuilder =
      add("else ")._ifLTE(left, right)(f)

    def assign(left: Any, right: Any): Unit =
      addLine(s"${left} = ${right}")

    def block[T](f: => T): CodeBuilder = {
      addLine(" {")
      indentation += 1
      f
      submitLine()
      indentation -= 1
      addLine("}")
    }

    def build(): String = {
      submitLine()
      lines.mkString("\n")
    }

  }

  trait Pattern {
    def |(that: Pattern) = Or(this, that)

    def >>(that: Pattern) = Seq_(this, that)

    def many(): Pattern = Many(this)

    def many1(): Pattern = this >> many
  }

  case object None_ extends Pattern
  case object Pass  extends Pattern

  case class Ran(start: Int, end: Int) extends Pattern

  case class Or(left: Pattern, right: Pattern) extends Pattern

  case class Seq_(first: Pattern, second: Pattern) extends Pattern

  case class Many(body: Pattern) extends Pattern

  class Rule[T](val expr: Pattern, var fn: Func0[T]) {

    def run(f: Func0[T]): Unit = {
      fn = f
    }
  }

//  object ParserBase {

  trait Result[T] {
    def map[S](fn: T => S): Result[S]
  }

  final case class Success[T](value: T) extends Result[T] {
    def map[S](fn: T => S) = Success(fn(value))
  }
  final case class Partial[T](value: T, offset: Int) extends Result[T] {
    def map[S](fn: T => S) = Partial(fn(value), offset)
  }
  final case class Failure[T](value: T, offset: Int) extends Result[T] {
    def map[S](fn: T => S) = Failure(fn(value), offset)
  }
//  }

  abstract class ParserBase[T] {
//    import ParserBase._

    import java.io.Reader
    import java.io.StringReader
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

    var result: T

    def run(input: String): Result[T] = {
      description() // Register group names etc.
      initialize()
      sreader = new StringReader(input)
      val numRead = sreader.read(buffer, 0, buffer.length)
      bufferLen   = numRead
      currentChar = getNextChar
      var r = -1
      while (r == -1) {
        r = step()
      }

      if (offset >= bufferLen) Success(result)
      else if (r == -2) Failure(result, offset)
      else Partial(result, offset)
    }

    // Group management

    var groupStack: List[Int]                   = Nil
    val groupLabelMap: mutable.Map[Int, String] = mutable.Map()
    var group: Int                              = 0

    def beginGroup(group: Group[_]): Unit =
      throw new Exception("Should never be used without desugaring.")

    def beginGroup(g: Int): Unit = {
      println(s"Begin ${groupLabel(g)}")
      groupStack :+= group
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

    def description(): Unit
    def initialize(): Unit

    var groupsx = new ArrayBuffer[Group[_]]()

    def defineGroup[T](label: String = "unnamed"): Group[T] = {
      val groupIndex = groupsx.length
      val group      = new Group[T](groupIndex)
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
      logger.log(s"Next char '${escapeChar(nextChar)}'")
      nextChar
    }

    def specialize(): String = {
      description()

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

  class Group[T](val groupIx: Int) {
    val rules = ArrayBuffer[Rule[T]]()

    def rule(r: Rule[T]): Rule[T] = {
      rules.append(r)
      r
    }

    def rule(expr: Pattern): Rule[T] = rule(new Rule[T](expr, null))

    def cloneRulesFrom(that: Group[T]): Unit = {
      rules.appendAll(that.rules)
    }

    def ruleName(ruleIx: Int): String =
      s"group${groupIx}_rule$ruleIx()"

    def buildAutomata(): NFA = {
      val nfa   = new NFA
      val start = nfa.addState()
      val endpoints = rules.zipWithIndex.map {
        case (rule, ix) => buildRuleAutomata(nfa, start, ix, rule)
      }
      val end = nfa.addState()
      nfa.state(end).end = true
      for (endpoint <- endpoints) {
        nfa.link(endpoint, end)
      }
      nfa
    }

    def buildRuleAutomata(
      nfa: NFA,
      previous: Int,
      ruleIx: Int,
      rule: Rule[_]
    ): Int = {
      val end           = buildExprAutomata(nfa, previous, rule.expr)
      val resultCompute = "currentMatch = matchBuilder.result()"
      val ruleEval      = ruleName(ruleIx)
      val code          = s"{$resultCompute; $ruleEval}"
      nfa.state(end).end  = true
      nfa.state(end).code = code
      end
    }

    def buildExprAutomata(nfa: NFA, previous: Int, expr: Pattern): Int = {
      val current = nfa.addState()
      nfa.link(previous, current)
      expr match {
        case None_ => nfa.addState()
        case Pass  => current
        case Ran(start, end) => {
          val state = nfa.addState()
          nfa.link(current, state, start, end)
          state
        }
        case Seq_(first, second) => {
          val s1 = buildExprAutomata(nfa, current, first)
          buildExprAutomata(nfa, s1, second)
        }
        case Many(body) => {
          val s1 = nfa.addState()
          val s2 = buildExprAutomata(nfa, s1, body)
          val s3 = nfa.addState()
          nfa.link(current, s1)
          nfa.link(current, s3)
          nfa.link(s2, s3)
          nfa.link(s3, s1)
          s3
        }
        case Or(first, second) => {
          val s1 = buildExprAutomata(nfa, current, first)
          val s2 = buildExprAutomata(nfa, current, second)
          val s3 = nfa.addState()
          nfa.link(s1, s3)
          nfa.link(s2, s3)
          s3
        }
      }
    }

    def generate(): String = {
      val nfa = buildAutomata()
      nfa.computeIsos()
      val dfa  = nfa.computeDFA()
      var code = CodeGen(dfa).generate(groupIx)
      code += s"\n  groups($groupIx) = runGroup$groupIx"

      rules.zipWithIndex.foreach {
        case (rule, ruleIx) => {
          code += s"\n def ${ruleName(ruleIx)}() = {\n"
          code += rule.fn.toString + "\n}"
        }
      }
      code
    }
  }

}
