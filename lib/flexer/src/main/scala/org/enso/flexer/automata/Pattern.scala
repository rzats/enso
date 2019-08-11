package org.enso.flexer.automata

import org.enso.flexer.Parser

import scala.annotation.tailrec

trait Pattern {
  import Pattern._

  def |(that: Pattern):  Pattern = Or(this, that)
  def >>(that: Pattern): Pattern = Seq(this, that)
  def many:              Pattern = Many(this)
  def many1:             Pattern = this >> many
}

object Pattern {
  case object Never                               extends Pattern
  case object Always                              extends Pattern
  case class Range(start: Int, end: Int)          extends Pattern
  case class Or(left: Pattern, right: Pattern)    extends Pattern
  case class Seq(first: Pattern, second: Pattern) extends Pattern
  case class Many(body: Pattern)                  extends Pattern

  //// API ////

  val never: Pattern  = Never
  val always: Pattern = Always
  def range(start: Char, end: Char): Range = Range(start.toInt, end.toInt)
  def range(start: Int, end: Int):   Range = Range(start, end)
  def range(end: Int):               Range = range(0, end)
  def range(end: Char):              Range = range(0, end)
  def char(char: Char):              Range = range(char, char)
  def char(char: Int):               Range = range(char, char)

  val any: Range = range(Int.MaxValue)
  val eof: Range = char(Parser.eofCodePoint)

  def anyOf(chars: String):            Pattern = anyOf(chars.map(char))
  def anyOf(alts: scala.Seq[Pattern]): Pattern = alts.fold(never)(_ | _)

  def noneOf(chars: String): Pattern = {
    val pointCodes  = chars.map(_.toInt).sorted
    val startPoints = 0 +: pointCodes.map(_ + 1)
    val endPoints   = pointCodes.map(_ - 1) :+ Int.MaxValue
    val ranges      = startPoints.zip(endPoints)
    val validRanges = ranges.filter { case (s, e) => e >= s }
    val patterns    = validRanges.map { case (s, e) => range(s, e) }
    anyOf(patterns)
  }

  final def not(char: Char): Pattern =
    noneOf(char.toString)

//  def not2(pat: Pattern): Range = pat match {
//    case Never             => any
//    case Always            => Never
//    case Many(_)           => Never
//    case Range(start, end) =>
//  }

  def repeat(p: Pattern, min: Int, max: Int): Pattern = {
    val minPat = repeat(p, min)
    _repeatAlt(p, max - min, minPat, minPat)
  }

  def repeat(p: Pattern, num: Int): Pattern =
    _repeat(p, num, always)

  @tailrec
  final def _repeat(p: Pattern, num: Int, out: Pattern): Pattern = num match {
    case 0 => out
    case _ => _repeat(p, num - 1, out >> p)
  }

  @tailrec
  final def _repeatAlt(p: Pattern, i: Int, ch: Pattern, out: Pattern): Pattern =
    i match {
      case 0 => out
      case _ =>
        val ch2 = ch >> p
        _repeatAlt(p, i - 1, ch2, out | ch2)
    }

  implicit class ExtendedChar(_this: Char) {
    final def ||(that: Char): Pattern =
      Or(char(_this), char(that))
  }

  implicit def fromChar(char: Char): Pattern =
    range(char, char)
  implicit def fromString(str: String): Pattern = str.toList match {
    case Nil     => Always
    case s :: ss => ss.foldLeft(char(s): Pattern)(_ >> _)
  }
}
