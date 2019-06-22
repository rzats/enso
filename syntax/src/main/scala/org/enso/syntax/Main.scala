package org.enso.syntax

import java.io.Reader
import java.io.StringReader

import org.enso.syntax.text.lexer.Lexer
import org.enso.syntax.text.parser.Parser
import org.enso.syntax.text.parser.BParser
//import org.enso.syntax.text.parser.PP

import scala.language.implicitConversions

import org.enso.syntax.text.lexer.SParser

import scala.collection.mutable

import scala.reflect.runtime.currentMirror
import scala.tools.reflect.ToolBox

trait IsoComputeState
case object NotComputed extends IsoComputeState
case object InProgress  extends IsoComputeState
case object Computed    extends IsoComputeState

class Logger {
  var nesting = 0

  def log(msg: String): Unit =
    println("    " * nesting + msg)

  def group[T](msg: String)(f: () => T): T = {
    log(msg)
    nesting += 1
    val out = f()
    nesting -= 1
    out
  }
}

class State {
  val links: mutable.Map[Int, Int]       = mutable.Map()
  val isoLinks: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer()
  var isos: Set[Int]                     = Set()
  var isosId: Int                        = 0
  var isosComputed: IsoComputeState      = NotComputed
}

class NFA {
  val logger                             = new Logger()
  val states: mutable.ArrayBuffer[State] = new mutable.ArrayBuffer()
  val vocabulary: mutable.Set[Int]       = mutable.Set()
  val isoMap: mutable.Map[Set[Int], Int] = mutable.Map()

  def addState(): Int = {
    val state = new State()
    states += state
    states.length - 1
  }

  def link(start: Int, end: Int, char: Char): Unit = {
    val value = char.toInt
    vocabulary += value
    state(start).links += value -> end
  }

  def link(start: Int, end: Int): Unit =
    state(start).isoLinks += end

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
            s.isosId = isoMap.size
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
    logger.group("Computing NFA Matrix")(() => {
      val matrix = Array.ofDim[Int](states.length, vocabulary.size)
      for (stateIx <- states.indices) {
        val s = state(stateIx)
        for ((voc, vocIx) <- vocabulary.zipWithIndex) {
          s.links.get(voc) match {
            case Some(tgt) => matrix(stateIx)(vocIx) = tgt
            case None      => matrix(stateIx)(vocIx) = -1
          }
        }
      }
      matrix
    })
  }

  def computeDFAMatrix(): Unit = {
    logger.group("Computing DFA Matrix")(() => {
      val nfaMatrix  = computeNFAMatrix()
      var dfaRows    = 1
      var dfaMatrix  = Array.ofDim[Int](dfaRows, states.length)
      val ix         = 0
      val dfaIsoMap  = mutable.Map[Set[Int], Int]()
      val dfaIsoKeys = mutable.ArrayBuffer[Set[Int]]()

      val startIsos = state(0).isos

      dfaIsoKeys += startIsos
      dfaIsoMap += (startIsos -> 0)

      var i = 0
      while (i < dfaRows) {
        val isos = dfaIsoKeys(i)
        logger.group(s"Key '${isos}'")(() => {

          for ((voc, vocIx) <- vocabulary.zipWithIndex) {
            logger.group(s"Vocabulary '${voc.toChar}'")(() => {
              var tt = Set[Int]()
              isos.foreach(iso => {
                val tgt = nfaMatrix(iso)(vocIx)
                if (tgt != -1) {
                  tt = tt ++ state(tgt).isos
                }
                if (state(iso).links.contains(voc)) {}
              })
              logger.log(s"Result ${tt}")
              dfaIsoMap.get(tt) match {
                case Some(id) => logger.log(s"Existing DFA id ${id}")
                case None => {
                  val id = dfaIsoMap.size
                  dfaIsoMap += (tt -> id)
                  dfaIsoKeys += tt
                  dfaRows += 1
                  dfaMatrix :+ Array[Int](states.length)
                  logger.log(s"New DFA id ${id}")
                }
              }
            })
          }
        })
        i += 1
      }

//      for ((voc, vocIx) <- vocabulary.zipWithIndex) {
//        logger.group(s"Vocabulary '${voc.toChar}'")(() => {
//          var tt = Set[Int]()
//          state(ix).isos.foreach(iso => {
//            val tgt = nfaMatrix(iso)(vocIx)
//            if (tgt != -1) {
//              tt = tt ++ state(tgt).isos
//            }
//            if (state(iso).links.contains(voc)) {}
//          })
//          logger.log(s"Result ${tt}")
//          dfaIsoMap.get(tt) match {
//            case Some(id) => logger.log(s"Existing DFA id ${id}")
//            case None => {
//              val id = dfaIsoMap.size
//              dfaIsoMap += (tt -> id)
//              dfaIsoKeys += tt
//              dfaRows += 1
//              dfaMatrix :+ Array[Int](states.length)
//              logger.log(s"New DFA id ${id}")
//            }
//          }
//        })
//      }
    })
  }
}

object Main extends App {

  var indent = 0

  def showMatrix[T](matrix: Array[Array[T]]): String = {
    var repr = ""
    for (i <- matrix.indices) {
      val row = matrix(i)
      for (j <- row.indices) {
        val cell = row(j).toString()
        repr += cell + " " * (3 - cell.length)
      }
      repr += "\n"
    }
    repr
  }

  def pprint(s: String) {
    print("  " * indent)
    val (l, r2) = s.span(x => x != '(' && x != ')')
    print(l)
    if (r2 == "") {
      println
      return
    }

    val (m, r) = r2.splitAt(1)

    if (m == "(") {
      indent += 1
      println(m)
      pprint(r)
    }
    else if (m == ")") {
      indent -= 1
      println(m)
      pprint(r)
    }

  }

//  val str = "a (b"
  val str = "a\n b\n a" // .stripMargin
  println(str)
  val reader = new StringReader(str)
  val ss     = new Lexer(reader)
  val toks   = ss.lexAll()
  var ttt    = 10
  pprint(toks.toString)

  val sparser = new SParser(new StringReader(str))

  val bparser = new BParser(new StringReader(str))
  val parser  = new Parser(new StringReader(str))

  pprint(bparser.parse.toString())
  pprint(parser.parse.toString())
  pprint("!")
  println(sparser.strInput)
  pprint(sparser.parse.toString)

  val nfa = new NFA
  val s1  = nfa.addState()
  val s2  = nfa.addState()
  val s3  = nfa.addState()
  val s4  = nfa.addState()
  val s5  = nfa.addState()
  val s6  = nfa.addState()
  val s7  = nfa.addState()
  val s8  = nfa.addState()
  val s9  = nfa.addState()
  val s10 = nfa.addState()
  nfa.link(s1, s2)
  nfa.link(s2, s3)
  nfa.link(s2, s5)
  nfa.link(s3, s4, 'A')
  nfa.link(s3, s4, 'B')
  nfa.link(s4, s5)
  nfa.link(s5, s3)
  nfa.link(s5, s10)

  nfa.link(s1, s6)
  nfa.link(s6, s7)
  nfa.link(s6, s9)
  nfa.link(s7, s8, 'A')
  nfa.link(s7, s8, 'C')
  nfa.link(s8, s9)
  nfa.link(s9, s7)
  nfa.link(s9, s10)

  nfa.computeIsos()

  println(nfa.state(s1).isos)
  println(nfa.vocabulary)
  val m = nfa.computeNFAMatrix()
  println(showMatrix(m))

  nfa.computeDFAMatrix()

//  val toolbox = currentMirror.mkToolBox()
//
//  val as    = "2*(2+3)"
//  val compe = toolbox.eval(toolbox.parse(as))
//
//  println(compe.getClass) // prints class java.lang.Integer
//  println(compe) // prints 10

}
