package org.enso.syntax.text

import scala.annotation.switch

sealed trait X
final case class A() extends X
final case class B() extends X
final case class C() extends X

sealed trait Y
final case class YA(s: String) extends Y
final case class YB(s: String) extends Y
final case class YC(s: String) extends Y
object YA {
  def unapply(arg: Y): Option[String] = arg match {
    case t: YA => Some(t.s)
    case _     => None
  }
}
object YB {
  def unapply(arg: Y): Option[String] = arg match {
    case t: YB => Some(t.s)
    case _     => None
  }
}
object YC {
  def unapply(arg: Y): Option[String] = arg match {
    case t: YC => Some(t.s)
    case _     => None
  }
}

final case class Foo(i: Int)

class TestMe {
  val ff = 18

  def foo(t: X): Int = (t: @switch) match {
    case A() => 1
    case B() => 2
    case C() => 7
  }

  def foo2(t: Y): Int = (t: @switch) match {
    case YA(_) => 1
    case YB(_) => 2
    case YC(_) => 777
  }

  def bar(i: Int) = {
    (i: @switch) match {
      case 1 => println("1")
      case 2 => println("2")
      case _ => println("somethiddng else")
    }
  }

  trait T1[T] {
    def t1(t: T): Int
  }
  trait T2[T] {
    def t1(t: T): Int
  }

  implicit def _inst1: T1[Int] = _ => 5
  implicit val _inst2: T2[Int] = _ => 5

  def giveMe[T: T1](t: T)  = implicitly[T1[T]].t1(t)
  def giveMe2[T: T2](t: T) = implicitly[T2[T]].t1(t)

  val v1 = giveMe(5)
  val v2 = giveMe(6)

  val v3 = giveMe2(5)
  val v4 = giveMe2(6)

  def bar2(i: Int): Int = {
    val j = Foo(i)
    j.i
  }

}
