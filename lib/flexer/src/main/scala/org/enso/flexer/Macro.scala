package org.enso.flexer

import scala.reflect.macros.blackbox
import scala.reflect.macros.whitebox
import scala.reflect.macros.blackbox.Context
import scala.reflect.runtime.{universe => u}
import scala.reflect.api.Exprs

object Macro {

  def compile[T](p: () => ParserBase[T]): () => ParserBase[T] =
    macro compileImpl[T]

  def compileImpl[T: c.WeakTypeTag](
    c: Context
  )(p: c.Expr[() => ParserBase[T]]): c.Expr[() => ParserBase[T]] = {
    import c.universe._
    val tree   = p.tree
    val expr   = q"$tree()"
    val parser = c.eval(c.Expr[ParserBase[T]](c.untypecheck(expr.duplicate)))
    val groups = c.internal
      .createImporter(u)
      .importTree(u.Block(parser.groupsx.map(_.generate()): _*))

    val groupsRebind = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Select(Ident(base), name) =>
          println(showRaw(base), base.getClass.getName)
          val base2 = if (base.toString == "Parser") {
            q"this"
          } else Ident(base)
          super.transform(Select(base2, name))
        case node => super.transform(node)
      }
    }
    val groups2 = groupsRebind.transform(groups)

    val addGroupDefs = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Template(parents, self, body) =>
          val exprs = q"..$groups2;".asInstanceOf[Block].stats
          Template(parents, self, body ++ exprs)
        case node => super.transform(node)
      }
    }

//    val clsDef = c.parse(s"final class Generated extends $tree")
    val clsDef =
      c.parse(
        s"final class Generated extends $tree"
      )
    val tgtDef = addGroupDefs.transform(clsDef)
    val result = c.Expr[() => ParserBase[T]](q"""
         $tgtDef
         () => { new Generated () }
       """)
    println(result)
    result
  }

}
