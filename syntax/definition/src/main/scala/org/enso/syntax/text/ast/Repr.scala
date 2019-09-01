package org.enso.syntax.text.ast

import java.nio.charset.StandardCharsets

import org.enso.data.List1
import org.enso.data.Shifted
import cats.Monoid
import cats.implicits._

import scala.annotation.tailrec

////////////////////////////////////////////////////////////////////////////////
//// Repr //////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

object Repr {

  def apply[T: Repr.Of](t: T): Builder = implicitly[Repr.Of[T]].of(t)

  val R = Repr.Builder.Empty()

  //////////////////////////////////////////////////////////////////////////////
  //// Builder /////////////////////////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////

  sealed trait Builder {
    import Builder._

    val byteSpan: Int
    val span: Int

    def +[T: Repr.Of](that: T): Builder =
      Seq(this, implicitly[Repr.Of[T]].of(that))

    def ++[T: Repr.Of](that: T): Builder =
      this + " " + that

    def build(): String = {
      val bldr = new StringBuilder()
      @tailrec
      def go(lst: List[Builder]): Unit = lst match {
        case Nil =>
        case r :: rs =>
          r match {
            case r: Empty  => go(rs)
            case r: Letter => bldr += r.char; go(rs)
            case r: Space  => for (_ <- 1 to r.span) { bldr += ' ' }; go(rs)
            case r: Text   => bldr ++= r.str; go(rs)
            case r: _Seq   => go(r.first :: r.second :: rs)
          }
      }
      go(List(this))
      bldr.result()
    }
  }
  object Builder {

    final case class Empty() extends Builder {
      val byteSpan = 0
      val span     = 0
    }
    final case class Letter(char: Char) extends Builder {
      val byteSpan = char.toString.getBytes(StandardCharsets.UTF_8).length
      val span     = 1
    }
    final case class Space(span: Int) extends Builder {
      val byteSpan = span
    }
    final case class Text(str: String) extends Builder {
      val byteSpan = str.getBytes(StandardCharsets.UTF_8).length
      val span     = str.length
    }
    final case class _Seq(first: Builder, second: Builder) extends Builder {
      val byteSpan = first.byteSpan + second.byteSpan
      val span     = first.span + second.span
    }
    object Seq { def apply(l: Builder, r: Builder): Builder = l |+| r }

    implicit val monoidForBuilder: Monoid[Builder] = new Monoid[Builder] {
      def empty: Builder = R
      def combine(l: Builder, r: Builder): Builder = (l, r) match {
        case (_: Empty, t) => t
        case (t, _: Empty) => t
        case _             => _Seq(l, r)
      }
    }
  }

  //// Provider ////

  trait Provider {
    val repr: Builder
  }

  //// Implicits ////

  implicit def fromString(a: String): Builder = Repr(a)
  implicit def fromChar(a: Char):     Builder = Repr(a)

//  implicit def _Provider_[T: Repr.Of](t: T): Provider = of(t)

  //// Instances ////

  /////////////////////////////
  ///// Repr.Of Type Class ////
  /////////////////////////////

  trait Of[-T] {
    def of(a: T): Builder
  }
  object Of {
    def apply[T: Of]: Of[T] = implicitly[Of[T]]
  }
  def of[T](t: T)(implicit ev: Repr.Of[T])   = ev.of(t)
  def span[T](t: T)(implicit ev: Repr.Of[T]) = ev.of(t).span

  implicit class ToReprOps[T: Repr.Of](t: T) {
    def repr: Builder = Repr.of(t)
    def span: Int     = repr.span
  }

  ///// Instances ////

  implicit def _inst_0: Repr.Of[Unit]   = _ => Repr.Builder.Empty()
  implicit def _inst_1: Repr.Of[String] = Repr.Builder.Text(_)
  implicit def _inst_2: Repr.Of[Int] = {
    case 0 => R
    case i => Repr.Builder.Space(i)
  }
  implicit def _inst_3: Repr.Of[Char]    = Repr.Builder.Letter(_)
  implicit def _inst_4: Repr.Of[Builder] = identity(_)

  implicit def _inst_5[T1: Repr.Of, T2: Repr.Of]: Repr.Of[(T1, T2)] = {
    case (t1, t2) => Repr.Builder.Seq(Repr.of(t1), Repr.of(t2))
  }

  implicit def _inst_6[T <: Repr.Provider]: Repr.Of[T] = _.repr

  implicit def _inst_7[T: Repr.Of]: Repr.Of[List[T]] =
    _.map(_.repr).fold(R: Builder)(Repr.Builder.Seq(_, _))

  implicit def _inst_8[T: Repr.Of]: Repr.Of[List1[T]] =
    t => R + t.head + t.tail

  implicit def _inst_9[T: Repr.Of]: Repr.Of[Shifted[T]] =
    t => R + t.off + t.el

  implicit def _inst_10[T: Repr.Of]: Repr.Of[Shifted.List1[T]] =
    t => R + t.head + t.tail

  implicit def _inst_11[T: Repr.Of]: Repr.Of[Option[T]] =
    _.map(_.repr).getOrElse(R)

  implicit def _inst_12: Repr.Of[None.type] =
    _ => R

  implicit def _inst_13[T: Repr.Of]: Repr.Of[Some[T]] =
    _.map(_.repr).getOrElse(R)
}
