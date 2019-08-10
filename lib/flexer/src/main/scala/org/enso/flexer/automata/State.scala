package org.enso.flexer.automata

import org.feijoas.mango.common.{collect => Guava}

import scala.collection.mutable

class State {
  import State._

  val links: Link.Registry = new Link.Registry()

  var isos                                = Set[Int]()
  var isosId: Int                         = 0
  var isosComputed: State.IsoComputeState = State.NotComputed

  var start: Boolean = false
  var end: Boolean   = false
  var rule: String   = ""
}

object State {
  case class StateDesc(priority: Int, rule: String)

  trait IsoComputeState
  case object NotComputed extends IsoComputeState
  case object InProgress  extends IsoComputeState
  case object Computed    extends IsoComputeState

  object Link {
    class Registry {
      private type IntOrd = Ordering.Int.type
      val epsilon: mutable.ArrayBuffer[Int] = new mutable.ArrayBuffer()
      val ranged: Guava.mutable.RangeMap[Int, Int, IntOrd] =
        Guava.mutable.RangeMap()

      def add(target: Int): Unit =
        epsilon += target

      def add(target: Int, pattern: Range) =
        ranged.put(Guava.Range.closed(pattern.start, pattern.end), target)
    }
  }
}
