package org.enso.syntax.graph

import scala.collection.GenIterable
import scala.collection.IterableLike

object Utils {
  implicit class GenIterable_ops[A](seq: GenIterable[A]) {
    def mapPairs[B](f: (A, A) => B): GenIterable[B] =
      seq.zip(seq.drop(1)).map { case (a, b) => f(a, b) }
  }
}
