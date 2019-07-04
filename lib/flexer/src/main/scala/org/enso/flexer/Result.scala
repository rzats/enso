package org.enso.flexer

trait Result[T] {
  val offset: Int
  def map[S](fn: T => S): Result[S]
}

final case class Success[T](value: T, offset: Int) extends Result[T] {
  def map[S](fn: T => S) = Success(fn(value), offset)
}
final case class Partial[T](value: T, offset: Int) extends Result[T] {
  def map[S](fn: T => S) = Partial(fn(value), offset)
}
final case class Failure[T](value: T, offset: Int) extends Result[T] {
  def map[S](fn: T => S) = Failure(fn(value), offset)
}
final case class InternalFailure[T](offset: Int) extends Result[T] {
  def map[S](fn: T => S) = InternalFailure(offset)
}
