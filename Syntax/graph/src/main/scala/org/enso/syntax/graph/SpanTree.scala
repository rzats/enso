package org.enso.syntax.graph

import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Opr
import org.enso.syntax.text.ast.meta.Pattern

import scala.util.Success
import scala.util.Try

/** A GUI-friendly structure describing expression with nodes mapped to
  * expression text spans.
  *
  * SpanTree flattens nested applications into flat children lists and describes
  * possible actions, i.e. which nodes can be replaced when creating a
  * connection or where a new variable can be inserted.
  *
  * It is expected that this structure facilitates, with possible help of
  * additional layers, the following GUI parts:
  *  - expression editor and its behavior when creating a connection
  *  - node inputs
  *  - node outputs
  *
  * This structure can be considered a layer over AST that does not introduce
  * any new data, it just presents AST in more accessible form.
  */
sealed trait SpanTree {

  import SpanTree._

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

  /** Sequence of children and operations that can be performed on them. */
  def describeChildren: Seq[NodeInfo]

  /** Children nodes. */
  def children: Seq[SpanTree] = describeChildren.map(_.spanTree)

  /** Children that support [[Action.Set]] action. */
  def settableChildren: Seq[SpanTree] =
    describeChildren.filter(_.settable).map(_.spanTree)

  /** Children that support [[Action.Erase]] action. */
  def erasableChildren: Seq[SpanTree] =
    describeChildren.filter(_.erasable).map(_.spanTree)

  /** Index of the first character in the [[span]]. */
  def begin: TextPosition = span.begin

  /** Index of the first character after the [[span]] */
  def end: TextPosition = span.end

  /** Get node by traversing given path. */
  def get(path: Path): Try[SpanTree] = path match {
    case Seq() => Success(this)
    case topIndex :: tailIndices =>
      val tryChild = Try { children(topIndex) }
      tryChild.flatMap(_.get(tailIndices))
  }

  /** Calls function `f` for all nodes in the tree. */
  def foreach[U](f: NodeInfoWithPath => U): Unit = {
    def go(node: SpanTree, pathSoFar: Path): Unit =
      node.describeChildren.zipWithIndex.foreach {
        case (childInfo, index) =>
          val childPath = pathSoFar :+ index
          f(childInfo.addPath(childPath))
          go(childInfo.spanTree, childPath)
      }

    f(NodeInfoWithPath(this, RootPath, Actions.Root))
    go(this, RootPath)
  }

  def foldLeft[B](z: B)(op: (B, SpanTree.NodeInfoWithPath) => B): B = {
    var result = z
    foreach { nodeInfo =>
      result = op(result, nodeInfo)
    }
    result
  }

  def insertionPoints: Seq[TextPosition] = {
    foldLeft[Seq[TextPosition]](Seq()) {
      case (op, node) =>
        if (node.insertable)
          op :+ node.spanTree.begin
        else
          op
    }.sorted
  }
}

object SpanTree {

  /** Actions available for functions, i.e. application targets and infix
    * operators. While this API level allows setting functions, this capability
    * will likely be blocked in the GUI.
    */
  val functionActions: Set[Action] = Set(Action.Set)

  val rootActions: Set[Action] = Set[Action](Action.Set)

  /** Just a [[SpanTree]] with a sequence of [[Action]] that are supported for
    * it.
    */
  trait NodeInfoCommon {
    val spanTree: SpanTree
    val actions: Set[Action]

    def supports(action: Action): Boolean = actions.contains(action)
    def settable: Boolean                 = supports(Action.Set)
    def erasable: Boolean                 = supports(Action.Erase)
    def insertable: Boolean               = supports(Action.Insert)
  }

  /** Type used to describe node's children and actions it can be target of.
    * @see [[SpanTree.describeChildren]]
    */
  final case class NodeInfo(
    spanTree: SpanTree,
    actions: Set[Action] = Set()
  ) extends NodeInfoCommon {
    def addPath(path: SpanTree.Path): NodeInfoWithPath =
      NodeInfoWithPath(spanTree, path, actions)
  }

  /** Like [[NodeInfo]] but with [[Path]] - so it also allows to uniquely
    * denote the node position in the tree.
    */
  final case class NodeInfoWithPath(
    spanTree: SpanTree,
    path: SpanTree.Path,
    actions: Set[Action] = Set()
  ) extends NodeInfoCommon

  object NodeInfo {

    /** Create info for node that supports only a single [[Action]]. */
    def apply(spanTree: SpanTree, action: Action): NodeInfo =
      NodeInfo(spanTree, Set(action))

    /** Create info for node that supports all 3 [[Action]]s. */
    def withAllActions(spanTree: SpanTree): NodeInfo =
      NodeInfo(spanTree, Set(Action.Set, Action.Erase, Action.Insert))

    /** Create info for node that supports only [[Set]] [[Action]]. */
    def insertionPoint(position: TextPosition): NodeInfo =
      NodeInfo(EmptyEndpoint(position), Action.Insert)
  }

  /** Node that represent some non-empty AST. */
  final case class AstNodeInfo(
    position: TextPosition,
    ast: AST
  ) {}

  /** Node denoting a location that could contain some value but currently is
    * empty. Its span has length zero and it is not paired with any
    * AST. Typically it supports only [[Action.Insert]].
    *
    * E.g. `+` is an operator with two empty endpoints.
    */
  final case class EmptyEndpoint(position: TextPosition)
      extends SpanTree
      with Leaf {
    override def text: String   = ""
    override def span: TextSpan = TextSpan.Empty(position)
  }

  /** Node describing certain AST subtree, has non-zero span. */
  sealed trait AstNode extends SpanTree {
    def info: AstNodeInfo
    def ast: AST       = info.ast
    def text: String   = ast.show()
    def span: TextSpan = TextSpan(info.position, TextLength(ast))

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
    * @param firstOperand The left-most operand and the first input.
    *             // FIXME FIXME not really a self for right-assoc operators
    * @param parts Subsequent pairs of children (operator, operand).
    */
  case class OperatorChain(
    info: AstNodeInfo,
    opr: Opr,
    firstOperand: SpanTree,
    parts: Seq[(AstLeaf, SpanTree)]
  ) extends ApplicationLike {

    private def describeOperand(operand: SpanTree): NodeInfo = operand match {
      case _: EmptyEndpoint =>
        NodeInfo(operand, Action.Insert)
      case _ =>
        NodeInfo(operand, Actions.All)
    }

    override def describeChildren: Seq[SpanTree.NodeInfo] = {
      val leftmostOperandInfo = describeOperand(firstOperand)
      val childrenInfos = parts.foldLeft(Seq(leftmostOperandInfo)) {
        case (acc, (oprAtom, operand)) =>
          val operatorInfo = NodeInfo(oprAtom, functionActions)
          val operandInfo  = describeOperand(operand)
          acc :+ operatorInfo :+ operandInfo
      }

      // If we already miss last the argument, don't add additional placeholder.
      val insertAfterLast = childrenInfos.lastOption.map(_.spanTree) match {
        case Some(EmptyEndpoint(_)) => None
        case _                      => Some(NodeInfo.insertionPoint(end))
      }

      childrenInfos ++ insertAfterLast
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
    override def describeChildren: Seq[SpanTree.NodeInfo] = {
      val selfInfo = self match {
        case _: EmptyEndpoint => NodeInfo(self, Action.Insert)
        case _                => NodeInfo(self, Action.Set)
      }
      parts.foldLeft(Seq(selfInfo)) {
        case (acc, (oprAtom, operand)) =>
          val operatorInfo = NodeInfo(oprAtom)
          val operandInfo  = NodeInfo(operand)
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
    override def describeChildren: Seq[SpanTree.NodeInfo] = {
      val calleeInfo           = NodeInfo(callee, functionActions)
      val argumentsInfo        = arguments.map(NodeInfo.withAllActions)
      val potentialNewArgument = NodeInfo.insertionPoint(end)
      calleeInfo +: argumentsInfo :+ potentialNewArgument
    }
  }

  /** Helper trait to facilitate recognizing leaf nodes in span tree. */
  sealed trait Leaf {
    this -> SpanTree
    def describeChildren: Seq[SpanTree.NodeInfo] = Seq()
  }

  /** A leaf representing an AST element that cannot be decomposed any
    * further.
    */
  final case class AstLeaf(info: AstNodeInfo) extends AstNode with Leaf
  object AstLeaf {
    def apply(textPosition: TextPosition, ast: AST): SpanTree.AstLeaf =
      AstLeaf(AstNodeInfo(textPosition, ast))
  }

  /** Node representing a macro usage. */
  final case class MacroMatch(
    info: AstNodeInfo,
    prefix: Option[MacroSegment],
    segments: Seq[MacroSegment]
  ) extends AstNode {
    override def describeChildren: Seq[NodeInfo] = {
      prefix.map(NodeInfo(_)).toSeq ++ segments.map(NodeInfo(_))
    }
  }

  /** Node representing a macro prefix or segment. [[introducer]] is empty iff
    * represented node was prefix. Otherwise, it contains segment's head name.
    */
  final case class MacroSegment(
    override val begin: TextPosition,
    introducer: Option[String],
    patternMatch: Pattern.Match,
    override val describeChildren: Seq[NodeInfo]
  ) extends SpanTree {
    override def span: TextSpan = {
      val introLength = TextLength(introducer.map(_.length).getOrElse(0))
      TextSpan(begin, introLength + TextLength(patternMatch))
    }
    override def text: String =
      patternMatch.toStream.foldLeft(introducer.getOrElse(""))(
        (s, ast) => s + (" " * ast.off) + ast.el.show()
      )
  }

  /** An index in the sequence returned by [[SpanTree.children]] method. */
  type ChildIndex = Int

  /** A sequence of indices, describes a path through a [[SpanTree]].
    * @see [[ChildIndex]]
    */
  type Path = Seq[ChildIndex]

  /** [[Path]] to the root of the [[SpanTree]], i.e. empty path. */
  val RootPath: Path = Seq()
}
