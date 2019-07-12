package org.enso.flexer

import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => u}
import scala.reflect.api.Exprs

object Macro {

  def compile[T](p: ParserBase[T]): () => ParserBase[T] =
    macro compileImpl[T]

  def compileImpl[T: c.WeakTypeTag](
    c: Context
  )(p: c.Expr[ParserBase[T]]): c.Expr[() => ParserBase[T]] = {
    import c.universe._
    val parser = c.eval(c.Expr[ParserBase[T]](c.untypecheck(p.tree.duplicate)))
    val groups = c.internal
      .createImporter(u)
      .importTree(u.Block(parser.groupsx.map(_.generate()): _*))

    val err = new Error("Use the macro as `val cons = compile(new Parser())`")
    val base = p.tree match {
      case Apply(Select(New(base), _), _) => base
      case _                              => throw err
    }

    val addGroupDefs = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Template(parents, self, body) =>
          val exprs = q"..$groups;".asInstanceOf[Block].stats
          Template(parents, self, body ++ exprs)
        case node => super.transform(node)
      }
    }

    val clsdef  = q"class Generated extends $base".asInstanceOf[ClassDef]
    val clsdef2 = addGroupDefs.transform(clsdef)
    val result  = c.Expr[() => ParserBase[T]](q"""
         $clsdef2
         () => { new Generated () }
       """)
//    println(result)
    result
  }

  def compile2[T](p: () => ParserBase[T]): () => ParserBase[T] =
    macro compileImpl2[T]

  def compileImpl2[T: c.WeakTypeTag](
    c: Context
  )(p: c.Expr[() => ParserBase[T]]): c.Expr[() => ParserBase[T]] = {
    import c.universe._
    val tree   = p.tree
    val expr   = q"$tree()"
    val parser = c.eval(c.Expr[ParserBase[T]](c.untypecheck(expr.duplicate)))
    val groups = c.internal
      .createImporter(u)
      .importTree(u.Block(parser.groupsx.map(_.generate()): _*))

    val addGroupDefs = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Template(parents, self, body) =>
          val exprs = q"..$groups;".asInstanceOf[Block].stats
          Template(parents, self, body ++ exprs)
        case node => super.transform(node)
      }
    }

    val clsDef = c.parse(s"final class Generated extends $tree")
    val tgtDef = addGroupDefs.transform(clsDef)
    val result = c.Expr[() => ParserBase[T]](q"""
         $tgtDef
         () => { new Generated () }
       """)
    println(result)
    result
  }

}
