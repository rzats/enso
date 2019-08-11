package org.enso.flexer.automata

import org.enso.Logger
import org.enso.flexer.Vocabulary

import scala.collection.mutable

final class NFA {
  val logger: Logger                     = new Logger()
  val states: mutable.ArrayBuffer[State] = new mutable.ArrayBuffer()
  val vocabulary                         = new Vocabulary()

  //// API ////

  def addState(): Int = {
    val state = new State()
    states += state
    states.length - 1
  }

  def state(ix: Int): State =
    states(ix)

  def link(start: Int, end: Int, charRange: Range): Unit = {
    vocabulary.insert(charRange)
    state(start).links.add(end, charRange)
  }

  def link(start: Int, end: Int): Unit =
    state(start).links.add(end)

  //// NFA -> DFA ////

  final private class EpsMatrix {
    var links: Set[Int]   = Set()
    var computed: Boolean = false
  }

  private def fillEpsMatrix(i: Int, stateToMat: Array[EpsMatrix]): Unit = {
    val epsGroupIxMap: mutable.Map[Set[Int], Int] = mutable.Map()
    def go(i: Int): Unit = {
      var epsLinks = Set[Int](i)
      if (stateToMat(i) == null) {
        var circular  = false
        val epsMatrix = new EpsMatrix()
        stateToMat(i) = epsMatrix
        state(i).links.epsilon.foreach { tgt =>
          go(tgt)
          val tgtEpsMatrix = stateToMat(tgt)
          epsLinks = epsLinks + tgt ++ tgtEpsMatrix.links
          if (!tgtEpsMatrix.computed) {
            circular = true
          }
        }
        epsMatrix.links = epsLinks
        if (!circular) {
          if (epsGroupIxMap.get(epsLinks).isEmpty)
            epsGroupIxMap += (epsLinks -> epsGroupIxMap.size)
          epsMatrix.computed = true
        }
      }
    }
    go(i)
  }

  private def epsMatrix(): IndexedSeq[Set[Int]] = {
    val arr = new Array[EpsMatrix](states.size)
    states.indices.foreach(fillEpsMatrix(_, arr))
    arr.map(_.links)
  }

  private val matMissing = -1

  private def nfaMatrix(): Array[Array[Int]] = {
    logger.group("Computing NFA Matrix") {
      val matrix = Array.ofDim[Int](states.length, vocabulary.size)
      for (stateIx <- states.indices) {
        val s = state(stateIx)
        for ((range, vocIx) <- vocabulary) {
          s.links.ranged.get(range.start) match {
            case Some(tgt) => matrix(stateIx)(vocIx) = tgt
            case None      => matrix(stateIx)(vocIx) = matMissing
          }
        }
      }
      matrix
    }
  }

  def toDFA(): DFA = {
    logger.group("Computing DFA Matrix") {
      val epsMat    = epsMatrix()
      val nfaMat    = nfaMatrix()
      var dfaRows   = 0
      var dfaMat    = Array[Array[Int]]()
      val dfaEpsMap = mutable.Map[Set[Int], Int]()
      val dfaEpsIxs = mutable.ArrayBuffer[Set[Int]]()

      def addDFAKey(epsSet: Set[Int]): Int = {
        val id = dfaEpsMap.size
        dfaEpsMap += (epsSet -> id)
        dfaEpsIxs += epsSet
        dfaRows += 1
        dfaMat :+= Array.fill(vocabulary.size)(matMissing)
        logger.log(s"DFA[$id] = $epsSet")
        id
      }

      logger.group(s"Preparing start points") {
        val initEpsSet = epsMat(0)
        addDFAKey(initEpsSet)
      }

      var i = 0
      while (i < dfaRows) {
        val epsIxs = dfaEpsIxs(i)
        logger.group(s"Computing DFA[$i]") {
          for ((voc, vocIx) <- vocabulary) {
            logger.group(s"Vocabulary '$voc'") {
              var epsSet = Set[Int]()
              for (epsIx <- epsIxs) {
                val tgt = nfaMat(epsIx)(vocIx)
                if (tgt != matMissing)
                  epsSet = epsSet ++ epsMat(tgt)
              }
              if (epsSet.nonEmpty) {
                dfaMat(i)(vocIx) = dfaEpsMap.get(epsSet) match {
                  case None => addDFAKey(epsSet)
                  case Some(id) =>
                    logger.log(s"Existing DFA ID $id")
                    id
                }
              }
            }
          }
        }
        i += 1
      }

      val nfaEndStatePriorityMap = mutable.Map[Int, Int]()
      for (i <- nfaMat.indices) {
        if (state(i).end) {
          nfaEndStatePriorityMap += (i -> (nfaMat.length - i))
        }
      }

      val dfaEndStatePriorityMap = mutable.Map[Int, State.StateDesc]()
      for ((isos, dfaIx) <- dfaEpsIxs.zipWithIndex) {
        val iso = isos.maxBy(nfaEndStatePriorityMap.getOrElse(_, matMissing))
        nfaEndStatePriorityMap.get(iso) match {
          case None =>
          case Some(p) =>
            dfaEndStatePriorityMap += dfaIx -> State.StateDesc(
              p,
              state(iso).rule
            )
        }
      }
      DFA(vocabulary, dfaMat, dfaEndStatePriorityMap)
    }
  }

  def visualize(): String = {
    import java.awt.Desktop
    import java.net.URI
    import java.net.URLEncoder

    val gray  = "#AAAAAA"
    val lines = mutable.ArrayBuffer[String]()
    lines += "digraph G {"
    lines += "node [shape=circle width=0.8]"
    for ((state, source) <- states.zipWithIndex) {
      if (state.links.ranged.isEmpty) {
        lines += s"""$source [color="$gray" fontcolor="$gray"]"""
      } else {
        lines += s"""$source"""
      }
      for ((range, target) <- state.links.ranged.asMapOfRanges()) {
        lines += s"""$source -> $target [label="$range"]"""
      }
      for (target <- state.links.epsilon) {
        lines += s"""$source -> $target [style="dashed" color="$gray"]"""
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
}
