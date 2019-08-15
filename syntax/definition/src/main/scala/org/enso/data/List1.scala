package org.enso

import cats.data.NonEmptyList

package object data {
  type List1[+T] = NonEmptyList[T]
  object List1 {
    def apply[T](el: T, tail: List[T]): List1[T] = new List1(el, tail)
    def apply[T](el: T, tail: T*):      List1[T] = new List1(el, tail.toList)

    def unapply[T](t: List1[T]): Option[(T, List[T])] = Some((t.head, t.tail))

    implicit class List1_ops[+T](lst: NonEmptyList[T]) {
      def mapInit[B >: T](f: T => B): NonEmptyList[B] =
        if (lst.tail.isEmpty) lst
        else List1(f(lst.head), lst.tail.init.map(f) :+ lst.tail.last)

      def mapLast[B >: T](f: T => B): NonEmptyList[B] =
        if (lst.tail.isEmpty) List1(f(lst.head), lst.tail)
        else List1(lst.head, lst.tail.init :+ f(lst.tail.last))
    }

  }
}
