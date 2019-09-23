package org.enso.syntax.graph

import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Opr
import org.enso.syntax.text.ast.meta.Pattern

import scala.util.Success
import scala.util.Try

/** Span tree describes structure of a given Luna expression fragment. This is
  * the base type for span tree nodes.
  *
  * The span tree's structure is more friendly for GUI to consume than pure
  * AST, as the chains of prefix applications and operator calls are flattened.
  *
  * The tree is supposed to work both for the node expression (assignment's
  * right side) and pattern (left side).
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

  /** Returns list of children that can be target of [[Action.Erase]].
    */
  def erasableChildren: Seq[SpanTree] =
    describeChildren.filter(_.erasable).map(_.spanTree)

  /** Index of the first character in the [[span]]. */
  def begin: TextPosition = span.begin

  /** Index of the first character after the [[span]] */
  def end: TextPosition = span.end

  /** Get node by traversing given path. */
  def get(path: SpanTree.Path): Try[SpanTree] = path match {
    case Seq()                   => Success(this)
    case topIndex :: tailIndices => children(topIndex).get(tailIndices)
  }

  /** Calls function `f` for all nodes in the subtree. */
  def foreach[U](
    f: SpanTree.PathedChildInfo => U,
    pathSoFar: SpanTree.Path = Seq()
  ): Unit = describeChildren.zipWithIndex.foreach {
    case (childInfo, index) =>
      val childPath = pathSoFar :+ index
      f(childInfo.addPath(childPath))
      childInfo.spanTree.foreach(f, childPath)
  }

  def foldLeft[B](z: B)(op: (B, SpanTree.PathedChildInfo) => B): B = {
    var result = z
    this.foreach(x => result = op(result, x), Seq())
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

  /** Just a [[SpanTree]] with a sequence of [[Action]] that are supported for
    * it.
    */
  trait ChildInfoBase {
    val spanTree: SpanTree
    val actions: Set[Action]

    def supports(action: Action): Boolean = actions.contains(action)
    def settable:                 Boolean = supports(Action.Set)
    def erasable:                 Boolean = supports(Action.Erase)
    def insertable:               Boolean = supports(Action.Insert)
  }

  final case class ChildInfo(
    spanTree: SpanTree,
    actions: Set[Action] = Set()
  ) extends ChildInfoBase {
    def addPath(path: SpanTree.Path): PathedChildInfo =
      PathedChildInfo(spanTree, path, actions)
  }

  final case class PathedChildInfo(
    spanTree: SpanTree,
    path: SpanTree.Path,
    actions: Set[Action] = Set()
  ) extends ChildInfoBase

  object ChildInfo {

    /** Create info for node that supports all 3 [[Action]]s. */
    def withAllActions(spanTree: SpanTree): ChildInfo =
      ChildInfo(spanTree, Set(Action.Set, Action.Erase, Action.Insert))

    /** Create info for node that supports only [[Set]] [[Action]]. */
    def insertionPoint(position: TextPosition): ChildInfo =
      ChildInfo(EmptyEndpoint(position), Set(Action.Insert))
  }

  /** Node that represent some non-empty AST. */
  final case class AstNodeInfo(
    position: TextPosition,
    ast: AST
  ) {}

  /** Node denoting a location that could contain some value but currently is
    * empty. Its span has length zero and it is not paired with any
    * AST.
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
    def ast:  AST      = info.ast
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

    private def describeOperand(operand: SpanTree): ChildInfo = operand match {
      case _: EmptyEndpoint =>
        ChildInfo(operand, Set(Action.Insert))
      case _ =>
        ChildInfo(operand, Action.All)
    }

    override def describeChildren: Seq[SpanTree.ChildInfo] = {
      val leftmostOperandInfo = describeOperand(firstOperand)
      val childrenInfos = parts.foldLeft(Seq(leftmostOperandInfo)) {
        case (acc, (oprAtom, operand)) =>
          val operatorInfo = ChildInfo(oprAtom)
          val operandInfo  = describeOperand(operand)
          acc :+ operatorInfo :+ operandInfo
      }

      // If we already miss last the argument, don't add additional placeholder.
      val insertAfterLast = childrenInfos.lastOption.map(_.spanTree) match {
        case Some(EmptyEndpoint(_)) => None
        case _                      => Some(ChildInfo.insertionPoint(end))
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
    override def describeChildren: Seq[SpanTree.ChildInfo] = {
      val selfInfo = self match {
        case _: EmptyEndpoint => ChildInfo(self, Set(Action.Insert))
        case _                => ChildInfo(self, Set(Action.Set))
      }
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
    override def describeChildren: Seq[ChildInfo] = {
      prefix.map(ChildInfo(_)).toSeq ++ segments.map(ChildInfo(_))
    }
  }

  /** Node representing a macro prefix or segment. [[introducer]] is empty iff
    * represented node was prefix. Otherwise, it contains segment's head name.
    */
  final case class MacroSegment(
    override val begin: TextPosition,
    introducer: Option[String],
    patternMatch: Pattern.Match,
    override val describeChildren: Seq[ChildInfo]
  ) extends SpanTree {
    override def span: TextSpan = {
      val introLength = TextLength(introducer.map(_.length).getOrElse(0))
      TextSpan(begin, introLength + TextLength(patternMatch))
    }
    override def text: String =
      patternMatch.toStream.foldLeft(introducer.getOrElse(""))(
        (s, ast) => s + (" " * ast.off).toString + ast.el.show()
      )
  }

  /** An index in the sequence returned by [[SpanTree.children]] method. */
  type ChildIndex = Int

  /** A sequence of child indices, describes a path through a SpanTree.. */
  type Path = Seq[ChildIndex]
}
