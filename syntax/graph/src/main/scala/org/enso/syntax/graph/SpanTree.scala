package org.enso.syntax.graph

import org.enso.syntax.graph.API.TextPosition
import org.enso.syntax.graph.API.TextSpan
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Opr

import scala.util.Success
import scala.util.Try

/** Span tree describes structure of a given Luna expression fragment. This is
  * the base type for span tree nodes.
  *
  * The span tree's structure is more friendly for GUI to work with than pure
  * AST, as the chains of prefix applications and operator calls are flattened.
  *
  * The tree is supposed to work both for the node expression (assignment's
  * right side) and pattern (left side). More specific structures describing
  * node's input and output port structures can be obtained from span tree
  * using [[OutputTree]] and [[InputTree]].
  */
sealed trait SpanTree {

  /** The text of expression part being described by this node. May be empty, if
    * this node represents just a point in text.
    */
  def text: String

  /** The span of expression part being described by this node. Indices are
    * relative to the beginning of the expression described by the tree root.
    *
    * For some nodes, like [[SpanTree.EmptyEndpoint]] span can be of zero
    * length — in such case they only represent a point within a string.
    */
  def span: TextSpan

  /** Returns lists of children and operations that can be performed on them.
    */
  def describeChildren: Seq[SpanTree.ChildInfo]

  /** Children nodes. This list contains a little more than GUI will typically
    * need, e.g. non-connectible parts of expressions (like function names and
    * operator names) are also included.
    */
  def children: Seq[SpanTree] = describeChildren.map(_.spanTree)

  /** Returns list of nodes supporting Set operations. If SpanTree describes a
    * node expression, then these are the parts that can be replaced with a Var
    * when creating a connection.
    */
  def settableChildren: Seq[SpanTree] =
    describeChildren.filter(_.settable).map(_.spanTree)

  /** Index of the first character in the [[span]]. */
  def begin: TextPosition = span.begin

  /** Index of the first character after the [[span]] */
  def end: TextPosition = span.end

  /** Get node by traversing given path. */
  def get(path: SpanTree.Path): Try[SpanTree] = path match {
    case Seq()                   => Success(this)
    case topIndex :: tailIndices => children(topIndex).get(tailIndices)
  }
}

object SpanTree {

  /** Just a [[SpanTree]] with a sequence of [[Action]] that are supported for
    * it.
    */
  final case class ChildInfo(
    spanTree: SpanTree,
    actions: Set[SpanTree.Action] = Set()
  ) {
    def supports(action: Action): Boolean = actions.contains(action)
    def settable:                 Boolean = supports(Action.Set)
    def erasable:                 Boolean = supports(Action.Erase)
    def insertable:               Boolean = supports(Action.Insert)

  }
  object ChildInfo {

    /** Create info for node that supports all 3 [[Action]]s. */
    def withAllActions(spanTree: SpanTree): ChildInfo =
      ChildInfo(spanTree, Set(Action.Set, Action.Erase, Action.Insert))

    /** Create info for node that supports only [[Set]] [[Action]]. */
    def insertionPoint(position: TextPosition): ChildInfo =
      ChildInfo(EmptyEndpoint(position), Set(Action.Insert))
  }

  /** Base type for all Actions that can be performed on [[SpanTree]] node in
    * the context of node expression.
    */
  sealed trait Action

  object Action {

    /** Insert is operation available for nodes with variable number of
      * children. Inserting on index of a child will shift it (and all
      * subsequent elements) right.
      */
    object Insert extends Action

    /** Erase is an operation to remove a child of node with variable children
      * count.
      */
    object Erase extends Action

    /** Set means replacing given node's expression part with a Var when
      * creating a connection.
      */
    object Set extends Action
  }

  /** Node that represent some non-empty AST. */
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
  final case class EmptyEndpoint(position: TextPosition)
      extends SpanTree
      with Leaf {
    override def text: String       = ""
    override def span: API.TextSpan = TextSpan(position, 0)
  }

  /** Node describing certain AST subtree, has non-zero span. */
  sealed trait AstNode extends SpanTree {
    def info: AstNodeInfo
    def ast:  AST      = info.ast
    def text: String   = ast.show()
    def span: TextSpan = TextSpan(info.position, ast.repr.span)

  }

  /** Generalization over prefix application (e.g. `print "Hello"`) and
    * infix operator application (e.g. `a + b`).
    */
  abstract class ApplicationLike extends AstNode {}

  /** E.g. `a + b + c` flattened to a single 5-child node. Operands can be
    * set and erased. New operands can be inserted next to existing operands.
    *
    * This is used for all infix operators other than dot that is represented
    * using [[AccessorPath]] node.
    *
    * @param opr The infix operator on which we apply operands. If there are
    *            multiple operators in chain, any of them might be referenced.
    * @param self The left-most operand and the first input.
    *             // FIXME FIXME not really a self for right-assoc operators
    * @param parts Subsequent pairs of children (operator, operand).
    */
  case class OperatorChain(
    info: AstNodeInfo,
    opr: Opr,
    self: SpanTree,
    parts: Seq[(AstLeaf, SpanTree)]
  ) extends ApplicationLike {
    override def describeChildren: Seq[SpanTree.ChildInfo] = {
      // FIXME not really a self, rename / reconsider
      val selfInfo = ChildInfo.withAllActions(self)
      val childrenInfos = parts.foldLeft(Seq(selfInfo)) {
        case (acc, (oprAtom, operand)) =>
          val operatorInfo = ChildInfo(oprAtom)
          val operandInfo  = ChildInfo.withAllActions(operand)
          acc :+ operatorInfo :+ operandInfo
      }
      childrenInfos :+ ChildInfo.insertionPoint(end)
    }
  }

  /** Just like [[OperatorChain]] but for dot `.` operator. Such operator chain
    * represents an access path. Only the first element ([[self]]) can be set.
    */
  case class AccessorPath(
    info: AstNodeInfo,
    opr: Opr,
    self: SpanTree,
    parts: Seq[(AstLeaf, SpanTree)]
  ) extends ApplicationLike {
    override def describeChildren: Seq[SpanTree.ChildInfo] = {
      val selfInfo = ChildInfo.withAllActions(self)
      parts.foldLeft(Seq(selfInfo)) {
        case (acc, (oprAtom, operand)) =>
          val operatorInfo = ChildInfo(oprAtom)
          val operandInfo  = ChildInfo(operand)
          acc :+ operatorInfo :+ operandInfo
      }
    }
  }

  /** E.g. `foo bar baz` flattened to a single 3-child node.
    *
    * The first child is a function name (calleee).
    * The second child is a "self" port.
    * Any additional children are flattened further arguments.
    */
  case class ApplicationChain(
    info: AstNodeInfo,
    callee: SpanTree,
    arguments: Seq[SpanTree]
  ) extends ApplicationLike {
    override def describeChildren: Seq[SpanTree.ChildInfo] = {
      val calleeInfo           = ChildInfo(callee)
      val argumentsInfo        = arguments.map(ChildInfo.withAllActions)
      val potentialNewArgument = ChildInfo.insertionPoint(end)
      Seq(calleeInfo) ++ argumentsInfo :+ potentialNewArgument
    }
  }

  /** Helper trait to facilitate recognizing leaf nodes in span tree. */
  sealed trait Leaf {
    this -> SpanTree
    def describeChildren: Seq[SpanTree.ChildInfo] = Seq()
  }

  /** A leaf representing an AST element that cannot be decomposed any
    * further.
    */
  case class AstLeaf(info: AstNodeInfo) extends AstNode with Leaf {}
  object AstLeaf {
    def apply(textPosition: TextPosition, ast: AST): SpanTree.AstLeaf =
      AstLeaf(AstNodeInfo(textPosition, ast))
  }

  /** An index in the sequence returned by [[SpanTree.children]] method. */
  type ChildIndex = Int

  /** A sequence of child indices, describes a path through a SpanTree.. */
  type Path = Seq[ChildIndex]
}
