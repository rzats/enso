package org.enso.syntax.graph

/** Base type for all Actions that can be performed on [[SpanTree]] node in
  * the context of node expression.
  */
sealed trait Action

object Action {

  /** Insert operation for non-empty nodes denote inserting before them. If
    * insertion target is an [[SpanTree.EmptyEndpoint]], it will be replaced.
    */
  object Insert extends Action

  /** Erases a child from node with variable child count.  */
  object Erase extends Action

  /** Replaces given node. */
  object Set extends Action

}

object Actions {

  /** All possible actions. */
  val All: Set[Action] = Set(Action.Insert, Action.Erase, Action.Set)

  /** Actions available on nodes recognized as function name */
  val Function: Set[Action] = Set(Action.Set)

  /** Actions available for the root node in the SpanTree. */
  val Root: Set[Action] = Set(Action.Set)
}
