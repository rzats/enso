package org.enso.flexer

import org.feijoas.mango.common.collect.mutable.RangeMap

import scala.collection.mutable
import scala.reflect.runtime.universe._

class State {
  //  val links                         = mutable.SortedMap[Int, Int]()
  val isoLinks                            = new mutable.ArrayBuffer[Int]()
  var isos                                = Set[Int]()
  var isosId: Int                         = 0
  var isosComputed: State.IsoComputeState = State.NotComputed

  var start  = false
  var end    = false
  var rule = TermName("")
  val links2 = RangeMap[Int, Int, Ordering.Int.type]()
}

object State {

  case class StateDesc(priority: Int, rule: TermName)

  trait IsoComputeState

  case object NotComputed extends IsoComputeState

  case object InProgress extends IsoComputeState

  case object Computed extends IsoComputeState

}
