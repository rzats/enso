package org.enso.syntax.graph

import org.enso.syntax.graph.API.TextPosition
import org.enso.syntax.graph.API.TextSpan
import org.enso.syntax.graph.AstOps.GeneralizedInfix
import org.enso.syntax.graph.Utils._
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Opr

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

  /** The text of expression part being described by this node. */
  // FIXME remove or make optional
  def text: String

  /** The span of expression part being described by this node. Indices are
    * relative to the beginning of the expression described by the tree root.
    *
    * For some nodes, like [[SpanTree.EmptyEndpoint]] span can be of zero
    * length — in such case they only represent a point within a string.
    */
  def span: TextSpan

  /** Children nodes. This list contains a little more than GUI will typically
    * need, e.g. non-connectible parts of expressions (like function names and
    * operator names) are also included.
    */
  def children: Seq[SpanTree]

  /** Index of the first character in the [[span]]. */
  def begin: TextPosition = span.begin

  /** Index of the first character after the [[span]] */
  def end: TextPosition = span.end

  /** List of actions that are allowed on this node. */
  def allowedActions: Seq[SpanTree.Action] =
    setAction.toList ++ insertActions ++ eraseActions

  /** Obtains action if node [[isSettable]]. */
  def setAction: Option[SpanTree.Action.Set] =
    if (isSettable) Some(SpanTree.Action.Set(span))
    else None

  /** If the [[SpanTree.Action.Set]] can be performed for this node. Typically
    * equivalent to "can this node be replaced with [[AST.Var]]
    */
  def isSettable: Boolean

  /** Returns a sequence with insert actions allowed on this node. */
  def insertActions: Seq[SpanTree.Action.Insert] = Seq()

  /** Returns a sequence with erase actions allowed on this node. */
  def eraseActions: Seq[SpanTree.Action.Erase] = Seq()

  /** Returns children that GUI wants to display. Function names and similar
    * non-interactive leaves are filtered away. */
  def settableChildren: Seq[SpanTree] = children.filter(_.isSettable)
}

object SpanTree {
  sealed trait Action {
    def span: TextSpan
  }
  object Action {
    case class Insert(span: TextSpan, index: Int) extends Action
    case class Erase(span: TextSpan)              extends Action
    case class Set(span: TextSpan)                extends Action

    object Insert {
      def apply(pos: TextPosition, index: Int): Insert =
        Insert(TextSpan(pos, 0), index)
    }
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
    override def text:       String       = ""
    override def span:       API.TextSpan = TextSpan(position, 0)
    override def isSettable: Boolean      = true
  }

  /** Node describing certain AST subtree, has non-zero span. */
  sealed trait AstNode extends SpanTree {
    def info: AstNodeInfo
    def ast:  AST      = info.ast
    def text: String   = ast.show()
    def span: TextSpan = TextSpan(info.position, ast.repr.span)

  }

  /** Also apps with no arguments (like `foo` or `15`) and flattened chains
    * of applications
    */
  abstract class ApplicationLike extends AstNode {
    override def isSettable: Boolean = true
  }

  /** E.g. `a + b + c` flattened to a single 5-child node.
    *
    * First child is a "self" port.
    * Every second child is an operator. They are grouped in pairs
    * (operator, argument), in the example `(+,b)` and `(+, c)`.
    *
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
  }

  /** E.g. `foo bar baz` flattened to a single 3-child node.
    *
    * The first child is a function name (calleee).
    * The second child is a "self" port.
    * Any additional children are flattened further arguments.
    **/
  case class ApplicationChain(
    info: AstNodeInfo,
    callee: FunctionName,
    arguments: Seq[SpanTree]
  ) extends ApplicationLike {

    override def children: Seq[SpanTree] = callee +: arguments

    override def insertActions: Seq[SpanTree.Action.Insert] = {

      var index = 0
      def getIndex: Int = {
        val ret = index
        index += 1
        ret
      }

      val insertBeforeFirst = arguments.headOption.map { firstArgument =>
        Action.Insert(callee.end <-> firstArgument.begin, getIndex)
      }
      val insertBetween = arguments.mapPairs { (leftChild, rightChild) =>
        Action.Insert(leftChild.span.end <-> rightChild.span.begin, getIndex)
      }
      val insertAfterLast = Action.Insert(end, getIndex)

      insertBeforeFirst.toSeq ++ insertBetween :+ insertAfterLast
    }

    override def eraseActions: Seq[Action.Erase] =
      arguments.map(Action Erase _.span)
  }

  /** Helper trait to facilitate recognizing leaf nodes in span tree. */
  sealed trait Leaf {
    this -> SpanTree
    def children: Seq[SpanTree] = Seq()
  }

  /** A leaf representing an AST element that cannot be decomposed any
    * further.
    **/
  case class Atom(info: AstNodeInfo) extends AstNode with Leaf {
    override def isSettable: Boolean = true
  }
  object Atom {
    def apply(textPosition: TextPosition, ast: AST): SpanTree.Atom =
      Atom(AstNodeInfo(textPosition, ast))
  }

  /** Function name is a [[AST.Var]] or [[AST.Opr]] -- i.e. the target of
    * an application. It is just like [[Atom]], however cannot be set.
    */
  case class FunctionName(info: AstNodeInfo) extends AstNode with Leaf {
    override def isSettable: Boolean = false
  }
  object FunctionName {
    def apply(atom: Atom): FunctionName =
      FunctionName(atom.info)
    def apply(textPosition: TextPosition, ast: AST): FunctionName =
      FunctionName(AstNodeInfo(textPosition, ast))
  }

  type Operator = FunctionName

  type Path = Seq[Int]
}

//////////////////////////
/// INPUTS //////////////
//////////////////////////

sealed trait InputTree {
  def path: SpanTree.Path
  def span: TextSpan
}
final case class InputGroup(
  path: SpanTree.Path,
  span: TextSpan,
  children: Seq[InputTree]
) extends InputTree

final case class InputLeafPort(path: SpanTree.Path, span: TextSpan)
    extends InputTree
final case class InsertionPoint(path: SpanTree.Path, span: TextSpan)
    extends InputTree

object InputTree {
  def apply(root: SpanTree, path: SpanTree.Path = Seq()): Option[InputTree] =
    root match {
      case chain: SpanTree.ApplicationLike =>
        val directChildren =
          chain.children.zipWithIndex.flatMap {
            case (node, index) if node.isSettable =>
              InputTree(node, path :+ index)
            case _ =>
              None
          }
        val insertionPoints = chain.insertActions.map { action =>
          InsertionPoint(path :+ action.index, action.span)
        }

        val children = (directChildren ++ insertionPoints).sortBy(_.span)
        Some(InputGroup(path, root.span, children))
      case leaf: SpanTree.Leaf if leaf.isSettable =>
        Some(InputLeafPort(path, leaf.span))
      case other =>
        println(s"observe me: $other")
        None
    }
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
    case atom: SpanTree.Atom =>
      atom.ast match {
        case AST.Var.any(var_) => Some(Var(var_))
        case _                 => None
      }
    case _ =>
      println(s"observe me: $node") // FIXME just see what are cases
      None
  }
}
