package org.enso.macros

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

object Func {
  implicit def apply[P, R](fn: P => R): Func[P, R] = macro apply_impl[P, R]

  def apply_impl[P: c.WeakTypeTag, R: c.WeakTypeTag](
    c: Context
  )(fn: c.Expr[P => R]): c.Expr[Func[P, R]] = {
    import c.universe._
    c.Expr(q" new Func($fn, ${show(fn.tree)})")
  }
}

class Func[P, R](val fn: P => R, description: String) extends Function1[P, R] {
  def apply(p: P):       R      = fn(p)
  override def toString: String = description
}
