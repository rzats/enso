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

  /** All possible actions. */
  val All: Set[Action] = scala.Predef.Set(Insert, Erase, Set)
  //                     ^^^ full name because Action.Set shadows the container
}
