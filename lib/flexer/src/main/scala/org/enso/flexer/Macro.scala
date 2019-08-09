package org.enso.flexer

object Macro {
  import scala.reflect.macros.blackbox.Context
  import scala.reflect.runtime.{universe => u}

  type Base[T] = ParserBase[T]
  type In[T]   = () => Base[T]
  type Out[T]  = () => Base[T]

  def compile[T](p: In[T]): Out[T] =
    macro compileImpl[T]

  def compileImpl[T: c.WeakTypeTag](
    c: Context
  )(p: c.Expr[In[T]]): c.Expr[Out[T]] = {
    import c.universe._
    val tree   = p.tree
    val expr   = q"$tree()"
    val parser = c.eval(c.Expr[Base[T]](c.untypecheck(expr.duplicate)))
    val groups = c.internal
      .createImporter(u)
      .importTree(u.Block(parser.groupsx.map(_.generate()): _*))

    val superClassName = tree match {
      case Select(_, name) => name
      case _ =>
        println("ERROR: Wrong shape")
        println("Expected Select(_, name), got:")
        println(showRaw(tree))
        throw new Error("Wrong shape")
    }

    val groupsRebind = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Select(Ident(base), name) =>
          val base2 = if (base == superClassName) q"this" else Ident(base)
          super.transform(Select(base2, name))
        case node => super.transform(node)
      }
    }

    val reboundGroups = groupsRebind.transform(groups)

    val addGroupDefs = new Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case Template(parents, self, body) =>
          val exprs = q"..$reboundGroups;None".asInstanceOf[Block].stats
          Template(parents, self, body ++ exprs)
        case node => super.transform(node)
      }
    }

    val clsDef = c.parse(s"final class __Parser__ extends $tree")
    val tgtDef = addGroupDefs.transform(clsDef)
    c.Expr[Out[T]](q"$tgtDef; () => { new __Parser__ () }")
  }

}

object Test {

  import scala.annotation.StaticAnnotation
  import scala.reflect.macros.blackbox.Context

  //  import scala.reflect.macros._
  import language.experimental.macros

  class foo extends StaticAnnotation {
    def macroTransform(annottees: Any*) = macro fooMacro.impl
  }

  object fooMacro {

    def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      val inputs: List[Tree] = annottees.map(_.tree)(collection.breakOut)
      println("------------------------------------------")
      println(inputs)
      val outputs: List[Tree] = inputs match {
        case (cd @ ClassDef(_, cName, _, _)) :: tail =>
          println("!!!!")
          val mod0: ModuleDef = tail match {
            case (md @ ModuleDef(_, mName, _)) :: Nil
                if cName.decoded == mName.decoded =>
              md
            case Nil =>
              val cMod  = cd.mods
              var mModF = NoFlags
              if (cMod hasFlag Flag.PRIVATE) mModF |= Flag.PRIVATE
              if (cMod hasFlag Flag.PROTECTED) mModF |= Flag.PROTECTED
              if (cMod hasFlag Flag.LOCAL) mModF |= Flag.LOCAL
              val mMod = Modifiers(mModF, cMod.privateWithin, Nil)
              // or should we have parents = List(AnyRef) and body = List(DefDef(???))
              val mTemp = Template(parents = Nil, self = noSelfType, body = Nil)
              val mName = TermName(cName.decoded) // or encoded?
              ModuleDef(mMod, mName, mTemp)
            case _ =>
              c.abort(c.enclosingPosition, "Expected a companion object")
          }
          val Template(mTempParents, mTempSelf, mTempBody0) = mod0.impl
          val fooDef = DefDef(
            NoMods,
            TermName("hasFoo"),
            Nil,
            Nil,
            TypeTree(typeOf[Int]),
            Literal(Constant(33))
          )
          val mTempBody1 = fooDef :: mTempBody0
          val mTemp1     = Template(mTempParents, mTempSelf, mTempBody1)
          val mod1       = ModuleDef(mod0.mods, mod0.name, mTemp1)
          cd :: mod1 :: Nil

        case _ => c.abort(c.enclosingPosition, "Must annotate a class or trait")
      }
      c.Expr[Any](Block(outputs, Literal(Constant(()))))
    }
  }

}
