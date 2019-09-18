package org.enso.syntax.graph

import org.enso.data.Shifted
import org.enso.syntax.text.AST

import scala.collection.GenIterable

object Ops {
  implicit class GenIterable_ops[A](seq: GenIterable[A]) {
    def mapPairs[B](f: (A, A) => B): GenIterable[B] =
      seq.zip(seq.drop(1)).map { case (a, b) => f(a, b) }
  }

  implicit class String_Span_ops(text: String) {
    def substring(span: TextSpan): String =
      text.substring(span.begin.index, span.end.index)
  }
}

/** Strongly typed index position in a text. */
case class TextPosition(index: Int) extends Ordered[TextPosition] {
  def +(offset: Int): TextPosition        = TextPosition(index + offset)
  def +(ast: AST): TextPosition           = this + ast.repr.span
  def +(sast: Shifted[AST]): TextPosition = this + sast.off + sast.el
  def +[A](elems: Seq[Shifted[AST]]): TextPosition = {
    // FIXME [mwu] can we just do dyanmic dispatch based on elem type?
    //  like as if we had two-phase lookupB
    var ret = this
    elems.foreach { elem =>
      ret = ret + elem
    }
    ret
  }

  /** Span between two text positions. Operands order is irrelevant. */
  def <->(that: TextPosition): TextSpan = TextSpan(this, that)

  def compare(rhs: TextPosition): Int = index compare rhs.index
}
object TextPosition {
  val Start = TextPosition(0)
}
case class TextSpan(begin: TextPosition, length: Int)
    extends Ordered[TextSpan] {

  /** Index of the first character past the span */
  def end: TextPosition = begin + length

  override def compare(that: TextSpan): Int =
    (begin -> length).compare(that.begin -> that.length)
}
object TextSpan {

  /** Span between two text positions. */
  def apply(pos1: TextPosition, pos2: TextPosition): TextSpan =
    if (pos1 > pos2) TextSpan(pos2, pos1)
    else TextSpan(pos1, pos2.index - pos1.index)
}
