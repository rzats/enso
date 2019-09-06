package org.enso.syntax.graph

import org.enso.syntax.graph.API.TextPosition
import org.enso.syntax.graph.API.TextSpan
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Opr

object SpanTree {
  trait Operation
  object Operation {
    object Insert extends Operation
    object Erase  extends Operation
    object Set    extends Operation
  }

  case class AstNodeInfo(
    textPosition: TextPosition,
    ast: AST,
    children: Seq[Node]
  ) {}

  sealed trait Flag
  object Self extends Flag

  sealed trait Node {
    def textPosition: TextPosition
    def text: String
    def span: TextSpan
    def children: Seq[Node]

    def supports(op: Operation): Boolean =
      // All nodes support setting. If node support more, it should override.
      op == Operation.Set

    // TODO below is rather GUI concern, now here for tests
    def visibleChildren: Seq[SpanTree.Node] = children
  }

  /** Node denoting a location that could contain some value but currently is
    * empty. Its span has length zero and it is not paired with any
    * AST.
    *
    * E.g. `+` is an operator with two empty endpoints.
    **/
  case class EmptyEndpoint(textPosition: TextPosition) extends Node {
    def text:     String       = ""
    def span:     API.TextSpan = TextSpan(textPosition, 0)
    def children: Seq[Node]    = Seq()
  }

  /** Node describing certain AST subtree, has non-zero span. */
  sealed trait AstNode extends Node {
    val info: AstNodeInfo
    def textPosition: TextPosition = info.textPosition
    def ast:          AST          = info.ast
    def children:     Seq[Node]    = info.children
    def text:         String       = ast.show()
    def span:         TextSpan     = TextSpan(textPosition, ast.repr.span)
  }

  /** Node with variable number of children. New one can be inserted on
    * after-last index. Existing ones can be erased.
    **/
  sealed trait InsertErase {
    this: Node =>
    override def supports(op: Operation): Boolean =
      this.supports(op) || op == Operation.Insert || op == Operation.Erase
  }

  /** Also apps with no arguments (like `foo` or `15`) and flattened chains
    * of applications
    * */
  abstract class ApplicationLike extends AstNode with InsertErase

  /** E.g. `a + b + c` flattened to a single 5-child node.
    *
    * First child is a "self" port.
    * Every second child is an operator.
    * All operators in the chain are expected to have the same name.
    **/
  case class OperatorChain(info: AstNodeInfo, opr: Opr)
      extends ApplicationLike {
    override def visibleChildren: Seq[Node] = children.filter {
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
    override def visibleChildren: Seq[Node] = children.drop(1)
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
