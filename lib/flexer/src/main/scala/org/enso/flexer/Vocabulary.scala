package org.enso.flexer

import org.enso.flexer.Vocabulary.Range

import scala.collection.immutable

class Vocabulary {
  var divisions = immutable.SortedSet[Int](0, Int.MaxValue)

  def insert(range: Range): Unit = {
    divisions = divisions + range.start
    divisions = divisions + (range.end + 1)
  }

  def size(): Int = divisions.size - 1

  override def toString: String =
    "Vocabulary(" + divisions.toList.map(_.toString).mkString(",") + ")"

  def iter[U]: Iterator[(Range, Int)] = {
    var lastDiv = 0
    for ((i, ix) <- divisions.iterator.drop(1).zipWithIndex) yield {
      val r = (Range(lastDiv, i - 1), ix)
      lastDiv = i
      r
    }
  }
}

object Vocabulary {

  case class Range(start: Int, end: Int)

}
