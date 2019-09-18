package org.enso.syntax.graph

// TODO this file contains totally provisional API to be rewritten

import org.enso.syntax.text.AST

//////////////////////////
/// INPUTS //////////////
//////////////////////////

sealed trait InputTree {
  def path: SpanTree.Path
}
final case class InputPort(
  path: SpanTree.Path,
  children: Seq[InputTree],
  settable: Boolean,
  erasable: Boolean
) extends InputTree
final case class InsertionPoint(path: SpanTree.Path) extends InputTree

object InputTree {
  def apply(root: SpanTree, path: SpanTree.Path = Seq()): Option[InputTree] = {
    None
  }

  //    root match {
  //      case chain: SpanTree.ApplicationLike =>
  //          chain.describeChildren.zipWithIndex.flatMap {
  //            case (node, index) => {
  //              val childPath = path :+ index
  //              val optInsertionPoint = if(node.insertable) Some(InsertionPoint(childPath)) else None
  //              val optSetPoint = if(node.settable) Some(InputGroup)
  //            }
  //              InputTree(node.spanTree, )
  //            case _ =>
  //              None
  //          }
  //        val insertionPoints = chain.insertActions.map { action =>
  //          InsertionPoint(path :+ action.index, action.span)
  //        }
  //
  //        val children = (directChildren ++ insertionPoints).sortBy(_.span)
  //        Some(InputGroup(path, root.span, children))
  //      case leaf: SpanTree.Leaf if leaf.isSettable =>
  //        Some(InputLeafPort(path, leaf.span))
  //      case other =>
  //        println(s"observe me: $other")
  //        None
  //    }
}

//////////////////////////
/// OUTPUTS //////////////
//////////////////////////

sealed trait OutputTree {
  def connectible: Boolean
}

object OutputTree {
  final case class PortGroup(children: Seq[OutputTree]) extends OutputTree {
    override def connectible: Boolean = false
  }

  /** An identifier introduced by the left-hand side of the node assignment. */
  final case class Var(name: String) extends OutputTree {
    override def connectible: Boolean = true
  }
  object Var { def apply(ast: AST.Var): Var = Var(ast.name) }

  /** Used to represent a potential output of a node that does not form an
    * assignment. Anonymous output can be connected and this will introduce a
    * new variable. For example, node from line `15` has anonymous output that
    * upon connecting will become `var1 = 15` - at which point output becomes a
    * [[OutputTree.Var]].
    */
  object Anonymous extends OutputTree {
    override def connectible: Boolean = true
  }

  def apply(optRoot: Option[SpanTree]): Option[OutputTree] = optRoot match {
    case Some(root) => OutputTree(root)
    case None       => Some(Anonymous)
  }

  def apply(node: SpanTree): Option[OutputTree] = node match {
    case infix: SpanTree.OperatorChain =>
      Some(PortGroup(infix.children.flatMap(OutputTree(_))))
    case atom: SpanTree.AstLeaf =>
      atom.ast match {
        case AST.Var.any(var_) => Some(Var(var_))
        case _                 => None
      }
    case _ =>
      println(s"observe me: $node") // FIXME just see what are cases
      None
  }
}
