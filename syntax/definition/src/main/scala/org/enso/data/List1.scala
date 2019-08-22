package org.enso

import cats.data.NonEmptyList

package object data {
  type List1[+T] = NonEmptyList[T]
  object List1 {
    def apply[T](el: T, tail: List[T]): List1[T] = new List1(el, tail)
    def apply[T](el: T, tail: T*):      List1[T] = new List1(el, tail.toList)

    def apply[T](list: List[T]):   Option[List1[T]] = fromListOption(list)
    def apply[T](array: Array[T]): Option[List1[T]] = fromIterableOption(array)

    def unapply[T](t: List1[T]): Option[(T, List[T])] = Some((t.head, t.tail))

    def fromIterableOption[T](itr: Iterable[T]): Option[List1[T]] =
      fromListOption(itr.toList)

    def fromListOption[T](lst: List[T]): Option[List1[T]] = lst match {
      case Nil     => None
      case t :: ts => Some(List1(t, ts))
    }

    implicit class List1_ops[+T](lst: List1[T]) {
      def mapInit[B >: T](f: T => B): List1[B] =
        if (lst.tail.isEmpty) lst
        else List1(f(lst.head), lst.tail.init.map(f) :+ lst.tail.last)

      def mapLast[B >: T](f: T => B): List1[B] =
        if (lst.tail.isEmpty) List1(f(lst.head), lst.tail)
        else List1(lst.head, lst.tail.init :+ f(lst.tail.last))

      def intersperse[B >: T](t: B): List1[B] =
        List1(lst.head, lst.tail.flatMap(s => List(t, s)))
    }

    implicit class List1_more_ops[T](lst: List1[T]) {
      def insert(index: Int, element: T): List1[T] = {
        if (index == 0)
          lst.prepend(element)
        else {
          def insertToList(list: List[T], i: Int) = {
            val (front, back) = list.splitAt(i)
            front ++ (element :: back)
          }
          List1(lst.head, insertToList(lst.tail, index - 1))
        }
      }

      def removeAt(index: Int): List[T] = {
        if (index == 0)
          lst.tail
        else
          lst.head :: lst.tail.patch(index - 1, Nil, 1)
      }

      def indexWhere(p: T => Boolean, from: Int = 0): Option[Int] =
        lst.toList.indexWhere(p, from) match {
          case -1    => None
          case index => Some(index)
        }
    }
  }
}
