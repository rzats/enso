package org.enso.syntax.graph

import org.enso.syntax.graph.API.TextPosition
import org.enso.syntax.graph.API.TextSpan
import org.enso.syntax.graph.AstOps.GeneralizedInfix
import org.enso.syntax.graph.Utils._
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Opr

sealed trait SpanTree {
  def position: TextPosition
  def text: String
  def span: TextSpan
  def children: Seq[SpanTree]

  def begin: TextPosition = span.begin
  def end:   TextPosition = span.end

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

  final case class AstNodeInfo(
    position: TextPosition,
    ast: AST
  ) {}

  sealed trait Flag
  object Self extends Flag

  /** Node denoting a location that could contain some value but currently is
    * empty. Its span has length zero and it is not paired with any
    * AST.
    *
    * E.g. `+` is an operator with two empty endpoints.
    */
  case class EmptyEndpoint(position: TextPosition) extends SpanTree {
    def text:     String        = ""
    def span:     API.TextSpan  = TextSpan(position, 0)
    def children: Seq[SpanTree] = Seq()
  }

  /** Node describing certain AST subtree, has non-zero span. */
  sealed trait AstNode extends SpanTree with Settable {
    def info: AstNodeInfo
    def children: Seq[SpanTree]
    def position: TextPosition = info.position
    def ast:      AST          = info.ast
    def text:     String       = ast.show()
    def span:     TextSpan     = TextSpan(position, ast.repr.span)

  }

  sealed trait Settable {
    this: SpanTree =>
    override def setAction: Option[SpanTree.Action] = Some(Action Set span)
  }

  /** Also apps with no arguments (like `foo` or `15`) and flattened chains
    * of applications
    */
  abstract class ApplicationLike extends AstNode {}

  /** E.g. `a + b + c` flattened to a single 5-child node.
    *
    * First child is a "self" port.
    * Every second child is an operator.
    * Further non-operator children are operands or empty endpoint.
    * All operators in the chain are expected to have the same name.
    **/
  case class OperatorChain(
    info: AstNodeInfo,
    opr: Opr,
    self: SpanTree,
    parts: Seq[(Operator, SpanTree)]
  ) extends ApplicationLike {

    override def children: Seq[SpanTree] = parts.foldLeft(Seq(self)) {
      case (acc, (oprAtom, operand)) => acc ++ Seq(oprAtom, operand)
    }

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
  case class ApplicationChain(
    info: AstNodeInfo,
    callee: SpanTree,
    arguments: Seq[SpanTree]
  ) extends ApplicationLike {

    override def children: Seq[SpanTree] = callee +: arguments

    /** Omits the function name child. */
    override def visibleChildren: Seq[SpanTree] = arguments

    override def insertEraseActions: Seq[SpanTree.Action] = {
      val erasures = this.children.map(Action Erase _.span)
      val insertFirst = children.headOption.map(
        firstChild => Action Insert TextSpan(this.position, firstChild.position)
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
  case class Atom(info: AstNodeInfo) extends AstNode {
    override def children: Seq[SpanTree] = Seq()
  }
  object Atom {
    def apply(textPosition: TextPosition, ast: AST): SpanTree.Atom =
      Atom(AstNodeInfo(textPosition, ast))
  }

  type Operator = Atom
}

sealed trait OutputInfo
final case class OutputPortGroup(children: Seq[OutputInfo]) extends OutputInfo
final case class OutputVar(name: String)                    extends OutputInfo
object AnonymousOutput                                      extends OutputInfo

object OutputInfo {
  def apply(optRoot: Option[SpanTree]): Option[OutputInfo] = optRoot match {
    case Some(root) => OutputInfo(root)
    case None       => Some(AnonymousOutput)
  }
  def apply(node: SpanTree): Option[OutputInfo] = node match {
    case infix: SpanTree.OperatorChain =>
      Some(OutputPortGroup(infix.children.flatMap(OutputInfo(_))))
    case atom: SpanTree.Atom =>
      atom.ast match {
        case AST.Var(name) => Some(OutputVar(name))
        case _             => None
      }
    case _ =>
      println("observe me") // FIXME just see what are cases
      None
  }
}
