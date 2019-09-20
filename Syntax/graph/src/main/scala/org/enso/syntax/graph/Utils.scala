package org.enso.syntax.graph

import org.enso.data.Shifted
import org.enso.syntax.text.AST
import org.enso.syntax.text.ast.meta.Pattern

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

object Utils {
  def canBeNothing(pat: Pattern): Boolean = {
    val emptyInput = List()
    pat.matchOpt(emptyInput, lineBegin = false, reversed = false) match {
      case None    => false
      case Some(_) => true
    }
  }
}

case class TextLength(value: Int) extends Ordered[TextLength] {
  def +(offset: TextLength):    TextLength = TextLength(value + offset.value)
  def compare(rhs: TextLength): Int        = value compare rhs.value
}
object TextLength {
  val Empty = TextLength(0)
  def apply(pat: Pattern.Match): TextLength = TextLength(pat.toStream)
  def apply(ast: AST):           TextLength = TextLength(ast.repr.span)
  def apply(text: String):       TextLength = TextLength(text.length)
  def apply(sast: Shifted[AST]): TextLength =
    TextLength(sast.off) + TextLength(sast.el)
  def apply[A](elems: Seq[Shifted[AST]]): TextLength = {
    // FIXME [mwu] can we just do dyanmic dispatch based on elem type?
    //  like as if we had two-phase lookupB
    var ret = TextLength(0)
    elems.foreach { elem =>
      ret += TextLength(elem)
    }
    ret
  }
}

/** Strongly typed index position in a text. */
case class TextPosition(index: Int) extends Ordered[TextPosition] {
  def +(offset: Int):        TextPosition = TextPosition(index + offset)
  def +(offset: TextLength): TextPosition = TextPosition(index + offset.value)
  def -(offset: TextLength): TextPosition = TextPosition(index - offset.value)

  /** Span between two text positions. Operands order is irrelevant. */
  def <->(that: TextPosition): TextSpan = TextSpan(this, that)

  def compare(rhs: TextPosition): Int = index compare rhs.index
}
object TextPosition {
  val Start = TextPosition(0)
}
case class TextSpan(begin: TextPosition, length: TextLength)
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
    else TextSpan(pos1, TextLength(pos2.index - pos1.index))

  def Empty(pos: TextPosition): TextSpan = TextSpan(pos, TextLength.Empty)
}
