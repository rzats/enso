package org.enso.flexer.automata

trait Pattern {
  import Pattern._

  def |(that: Pattern):  Pattern = Or(this, that)
  def >>(that: Pattern): Pattern = Seq(this, that)
  def many:              Pattern = Many(this)
  def many1:             Pattern = this >> many
}

object Pattern {
  case object Never                               extends Pattern
  case object Empty                               extends Pattern
  case class Range(start: Int, end: Int)          extends Pattern
  case class Or(left: Pattern, right: Pattern)    extends Pattern
  case class Seq(first: Pattern, second: Pattern) extends Pattern
  case class Many(body: Pattern)                  extends Pattern

  def range(start: Char, end: Char): Pattern = Range(start.toInt, end.toInt)
  def range(start: Int, end: Int):   Pattern = Range(start, end)
  def char(c: Char):                 Pattern = range(c, c)

  implicit class ExtendedChar(_this: Char) {
    final def ||(that: Char): Pattern =
      Or(char(_this), char(that))
  }

  implicit def fromChar(char: Char): Pattern =
    range(char, char)
  implicit def fromString(str: String): Pattern = str.toList match {
    case Nil     => Empty
    case s :: ss => ss.foldLeft(char(s))(_ >> _)
  }
}
