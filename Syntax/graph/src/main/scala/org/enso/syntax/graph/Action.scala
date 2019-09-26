package org.enso.syntax.graph

import org.enso.data.ADT

/** Base type for all Actions that can be performed on [[SpanTree]] node in
  * the context of node expression.
  */
sealed trait Action
object Action {
  object Insert extends Action
  object Erase  extends Action
  object Set    extends Action
}
object Actions {
  val All: Set[Action]      = ADT.constructors[Action]
  val None: Set[Action]     = Set()
  val Function: Set[Action] = Set(Action.Set)
  val Root: Set[Action]     = Set(Action.Set)
}
