package org.enso.flexer

trait Pattern {
  def |(that: Pattern) = Or(this, that)

  def >>(that: Pattern) = Seq_(this, that)

  def many(): Pattern = Many(this)

  def many1(): Pattern = this >> many
}

case object None_ extends Pattern
case object Pass  extends Pattern

case class Ran(start: Int, end: Int) extends Pattern

case class Or(left: Pattern, right: Pattern) extends Pattern

case class Seq_(first: Pattern, second: Pattern) extends Pattern

case class Many(body: Pattern) extends Pattern
