package org.enso.syntax.graph

import scala.collection.IterableLike

object Orphans {
  implicit class GenIterableOps[A, R](seq: IterableLike[A, R]) {

    /** Call `f` for all pairs of neighbouring elements.
      *
      * E.g. for Seq(1,2,3,4) `f` will be called with:
      *  - (1,2)
      *  - (2,3)
      *  - (3,4)
      */
    def mapPairs[B](f: (A, A) => B): Seq[B] = {
      val retItr = seq.sliding(2, 1).map { case Seq(a: A, b: A) => f(a, b) }
      retItr.toSeq
    }
  }
}
