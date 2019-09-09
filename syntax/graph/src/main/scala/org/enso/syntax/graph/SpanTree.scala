package org.enso.syntax.graph

import org.enso.syntax.graph.API.TextPosition
import org.enso.syntax.graph.API.TextSpan
import org.enso.syntax.graph.Utils._
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Opr

sealed trait SpanTree {
  import SpanTree._
  def textPosition: TextPosition
  def text: String
  def span: TextSpan
  def children: Seq[SpanTree]

  /** */
  def allowedConnectActions: Seq[SpanTree.Action] =
    setAction.toList ++ insertEraseActions

  def setAction:          Option[SpanTree.Action] = None
  def insertEraseActions: Seq[SpanTree.Action]    = Seq()

  // TODO below is rather GUI concern, now here for tests
  def visibleChildren: Seq[SpanTree] = children
}

object SpanTree {
  sealed trait Action {
    def span: TextSpan
  }
  object Action {
    case class Insert(span: TextSpan) extends Action
    case class Erase(span: TextSpan)  extends Action
    case class Set(span: TextSpan)    extends Action
  }

  case class AstNodeInfo(
    textPosition: TextPosition,
    ast: AST,
    children: Seq[SpanTree]
  ) {}

  sealed trait Flag
  object Self extends Flag

  /** Node denoting a location that could contain some value but currently is
    * empty. Its span has length zero and it is not paired with any
    * AST.
    *
    * E.g. `+` is an operator with two empty endpoints.
    */
  case class EmptyEndpoint(textPosition: TextPosition) extends SpanTree {
    def text:     String        = ""
    def span:     API.TextSpan  = TextSpan(textPosition, 0)
    def children: Seq[SpanTree] = Seq()
  }

  /** Node describing certain AST subtree, has non-zero span. */
  sealed trait AstNode extends SpanTree with Settable {
    val info: AstNodeInfo
    def textPosition: TextPosition  = info.textPosition
    def ast:          AST           = info.ast
    def children:     Seq[SpanTree] = info.children
    def text:         String        = ast.show()
    def span:         TextSpan      = TextSpan(textPosition, ast.repr.span)

  }

  sealed trait Settable {
    this: SpanTree =>
    def setAction: Option[SpanTree.Action] = Some(Action Set span)
  }

  /** Also apps with no arguments (like `foo` or `15`) and flattened chains
    * of applications
    */
  abstract class ApplicationLike extends AstNode {}

  /** E.g. `a + b + c` flattened to a single 5-child node.
    *
    * First child is a "self" port.
    * Every second child is an operator.
    * All operators in the chain are expected to have the same name.
    **/
  case class OperatorChain(info: AstNodeInfo, opr: Opr)
      extends ApplicationLike {
    override def visibleChildren: Seq[SpanTree] = children.filter {
      case astNode: AstNode =>
        astNode.ast match {
          case AST.Opr.any(childOpr) => childOpr.name != opr.name
          case _                     => true
        }
      case _ => true
    }
  }

  /** E.g. `foo bar baz` flattened to a single 3-child node.
    *
    * The first child is a function name (calleee).
    * The second child is a "self" port.
    * Any additional children are flattened further arguments.
    **/
  case class ApplicationChain(info: AstNodeInfo) extends ApplicationLike {

    /** Omits the function name child. */
    override def visibleChildren: Seq[SpanTree] = children.drop(1)

    override def insertEraseActions: Seq[SpanTree.Action] = {
      val erasures = this.children.map(Action Erase _.span)
      val insertFirst = children.headOption.map(
        firstChild =>
          Action Insert TextSpan(this.textPosition, firstChild.textPosition)
      )
      val insertLast = children.lastOption match {
        case Some(lastChildren) =>
          Action Insert TextSpan(lastChildren.span.end, span.end)
        case None =>
          Action Insert span
      }

      val insertBetween = children.mapPairs { (leftChild, rightChild) =>
        Action Insert TextSpan(leftChild.span.end, rightChild.span.begin)
      }

      erasures ++ insertFirst ++ Seq(insertLast) ++ insertBetween
    }
  }

  /** A leaf representing an AST element that cannot be decomposed any
    * further.
    **/
  case class Atom(info: AstNodeInfo) extends AstNode
  object Atom {
    def apply(textPosition: TextPosition, ast: AST): SpanTree.Atom =
      Atom(AstNodeInfo(textPosition, ast, Seq()))
  }
}
