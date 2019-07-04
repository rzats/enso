package org.enso.flexer

import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => u}

object Macro {

  def compile[T](p: ParserBase[T]): ParserBase[T] = macro compileImpl[T]

  def compileImpl[T: c.WeakTypeTag](
    c: Context
  )(p: c.Expr[ParserBase[T]]): c.Expr[ParserBase[T]] = {
    import c.universe._
    val parser = c.eval(c.Expr[ParserBase[T]](c.untypecheck(p.tree.duplicate)))
    val groups = c.internal
      .createImporter(u)
      .importTree(u.Block(parser.groupsx.map(_.generate()): _*))
    val result = q"import $p._; ..$groups; $p"
//    c.warning(c.enclosingPosition, showCode(result))
    c.Expr[ParserBase[T]](result)
  }
}
