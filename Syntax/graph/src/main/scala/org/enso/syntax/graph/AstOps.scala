package org.enso.syntax.graph

import org.enso.data.List1
import org.enso.data.List1.List1_ops
import org.enso.syntax.graph.API.Flag
import org.enso.syntax.graph.API.Module
import org.enso.syntax.graph.API.Node
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.App.Infix
import org.enso.syntax.text.AST.Block.Line
import org.enso.syntax.text.AST.Block.OptLine
import org.enso.syntax.text.AST.Import
import org.enso.syntax.text.AST.UnapplyByType
import org.enso.syntax.text.ast.opr.Assoc
import org.enso.syntax.text.ast.Repr._

import scala.collection.GenTraversableOnce

/** Operators with special meaning that we support. */
object KnownOperators {
  val Assignment = "="
  val Access     = "."
  val Minus      = "-"
}

object AstOps {
  implicit class Opr_ops(opr: AST.Opr) {
    def isAssignment: Boolean = opr.name == KnownOperators.Assignment
    def isAccess: Boolean     = opr.name == KnownOperators.Access
    def isMinus: Boolean      = opr.name == KnownOperators.Minus
  }

  implicit class AstOps_syntax_graph(ast: AST) {

    ////////////////////////////////////////////////////////
    ///// Parts to be moved to AST operations from here ////
    ////////////////////////////////////////////////////////
    case class MissingIdException(ast: AST) extends Exception {
      override def getMessage: String =
        s"missing id for node with expression `${ast.show()}`"
    }

    /** Gets [[AST.ID]] from this AST, throwing [[MissingIdException]] if ID
      * has not been set. */
    def unsafeID: AST.ID = ast.id.getOrElse(throw MissingIdException(ast))

    def as[T: UnapplyByType]: Option[T] = UnapplyByType[T].unapply(ast)
    def is[T: UnapplyByType]: Boolean   = ast.as[T].isDefined

    ////////////////////////////////////////////
    //// parts being specific to double rep ////
    ////////////////////////////////////////////

    def toImport: Option[AST.Import] =
      // FIXME [mwu] I doubt we can use here resolved macro
      as[Import].orElse(as[AST.Macro.Match].flatMap(_.resolved.toImport))

    def toAssignment: Option[AST.App.Infix] =
      ast.as[AST.App.Infix].filter(_.opr.isAssignment)

    /** If this AST is a [[AST.Var]], retrieves its name. */
    def toVarName: Option[String] = ast.as[AST.Var].map(_.name)

    /** Checks if this AST imports a given module. */
    def doesImport(module: Module.Name): Boolean = {
      ast.toImport.exists(_.path sameTarget module)
    }

    /** [[AST.Opr]] node when this AST is infix operator usage (full application
      * or a section.
      */
    def usedInfixOperator: Option[AST.Opr] =
      GeneralizedInfix(ast).map(_.operatorAst)

    def isInfixOperatorUsage: Boolean =
      ast.usedInfixOperator.nonEmpty

    def isInfixOperatorUsage(oprName: String): Boolean =
      ast.usedInfixOperator.exists(_.name == oprName)

    /** Linearizes nested sequence of App.Prefix.
      *
      * For example structures for code like `foo a b c` will be flattened to
      * sequence like Seq(foo, a, b, c).
      */
    def flattenPrefix(
      pos: TextPosition,
      ast: AST.App.Prefix
    ): Seq[(TextPosition, AST)] = {
      val init = ast.fn match {
        case AST.App.Prefix.any(lhsApp) => flattenPrefix(pos, lhsApp)
        case nonAppAst                  => Seq(pos -> nonAppAst)
      }
      val rhsPos = pos + ast.fn.span + ast.off
      val last   = rhsPos -> ast.arg
      init :+ last
    }
  }

  // FIXME: [mwu] is this safe if module name is List1 ?
  implicit class ModuleName_ops(moduleName: API.Module.Name) {
    def nameAsString(): String = moduleName.toList.map(_.name).mkString(".")

    /** Checks both names designate the same module.
      *
      * Can be non-trivial due to node comparing `id`.
      * FIXME [mwu] other changes made comparison id-insensitive, might not
      *  be needed anymore if change will be kept
      */
    def sameTarget(rhs: API.Module.Name): Boolean =
      moduleName.map(_.shape) == rhs.map(_.shape)
  }

  implicit class Module_ops(module: AST.Module) {
    def imports: List[Import] = module.flatTraverse(_.toImport)
    def lineIndexOf(ast: AST): Option[Int] =
      lineIndexWhere(_ == ast).map(_._2) // FIXME
    def lineIndexWhere(p: AST => Boolean): Option[(OptLine, Int)] =
      module.lines.zipWithIndex.find(_._1.elem.exists(p))

    def lineOffset(lineIx: Int): Int =
      module.lines.toList.take(lineIx).foldLeft(0) {
        case (offset, line) => offset + line.span
      }

    /** flatMaps non-empty lines ASTs. */
    def flatTraverse[B](f: AST => GenTraversableOnce[B]): List[B] =
      module.lines.toList.flatMap(_.elem).flatMap(f(_))

    def insert(index: Int, addedLine: OptLine): AST.Module = {
      val moduleIsEmpty = module.lines match {
        case List1(line, Nil) => line.elem.isEmpty
        case _                => false
      }
      val newLines =
        if (moduleIsEmpty) List1(addedLine)
        else module.lines.insert(index, addedLine)
      AST.Module(newLines)
    }
    def insert(index: Int, addedLineAst: AST): AST.Module =
      insert(index, OptLine(addedLineAst))

    def removeAt(index: Int): AST.Module = {
      val newLines = List1(module.lines.removeAt(index)) match {
        case Some(list1) => list1
        // if we removed the last line, restore an empty one
        case None => List1(Line(None))
      }
      AST.Module(newLines)
    }

    /** Calls function f on each line,
      *   - if f returns None, line is kept as it was,
      *   - if f returns Some, line is replaced with f
      *        and all further lines are kept as they were
      */
    def findAndReplace(f: OptLine => Option[List[OptLine]]): AST.Module = {
      def go(lines: List[OptLine]): List[OptLine] = lines match {
        case Nil => Nil
        case l +: ls =>
          f(l) match {
            case None           => l +: go(ls)
            case Some(newLines) => newLines ++ ls
          }
      }
      AST.Module(List1(go(module.lines.toList)).get)
    }
  }
}

//////////////////////////////////////////////////////////////////////////////
//// ExpressionPart //////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

/** Optional part of AST or point in it that knows its place in the
  * expression. Used e.g. to represent infix operator operands.
  */
sealed trait ExpressionPartOrPoint {
  def span:   TextSpan
  def astOpt: Option[AST]
}
object ExpressionPartOrPoint {
  def apply(pos: TextPosition, astOpt: Option[AST]): ExpressionPartOrPoint =
    astOpt match {
      case Some(ast) => ExpressionPart(pos, ast)
      case None      => EmptyPlace(pos)
    }
}

final case class ExpressionPart(pos: TextPosition, ast: AST)
    extends ExpressionPartOrPoint {
  override def span: TextSpan      = TextSpan(pos, ast)
  override def astOpt: Option[AST] = Some(ast)
}

/** Place in expression where some AST could have been present but was not. */
final case class EmptyPlace(pos: TextPosition) extends ExpressionPartOrPoint {
  override def span: TextSpan      = TextSpan(pos, TextLength.Empty)
  override def astOpt: Option[AST] = None
}

final case class InfixExpressionParts(
  left: ExpressionPartOrPoint,
  operator: ExpressionPart,
  right: ExpressionPartOrPoint
)

case class InfixChainPart(
  operator: ExpressionPart,
  operand: ExpressionPartOrPoint
)
final case class FlattenedInfixChain(
  self: ExpressionPartOrPoint,
  parts: Seq[InfixChainPart]
) {
  def toSeq: Seq[ExpressionPartOrPoint] = {
    val tail = parts.foldLeft(Seq[ExpressionPartOrPoint]()) { (acc, part) =>
      part.operator +: part.operand +: acc
    }
    self +: tail
  }
}

//////////////////////////////////////////////////////////////////////////////
//// GeneralizedInfix ////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////

/** A helper type to abstract away differences between proper infix operator
  * application and ones with missing argument (section apps). */
final case class GeneralizedInfix(
  leftArgAst: Option[AST],
  leftSpace: Int,
  operatorAst: AST.Opr,
  rightSpace: Int,
  rightArgAst: Option[AST]
) {
  import AstOps._

  def assoc: Assoc                = Assoc.of(operatorAst.name)
  def name: String                = operatorAst.name
  def span(ast: Option[AST]): Int = ast.map(_.span).getOrElse(0)

  def operatorOffset: Int = span(leftArgAst) + leftSpace
  def rightArgumentOffset: Int =
    operatorOffset + operatorAst.span + rightSpace

  def sameOperatorAsIn(ast: AST): Boolean =
    ast.isInfixOperatorUsage(operatorAst.name)

  def getParts(offset: TextPosition): InfixExpressionParts = {
    InfixExpressionParts(
      ExpressionPartOrPoint(offset, leftArgAst),
      ExpressionPart(offset + operatorOffset, operatorAst),
      ExpressionPartOrPoint(offset + rightArgumentOffset, rightArgAst)
    )
  }

  /** Converts nested operator applications into a flat list of all operands. */
  def flattenInfix(pos: TextPosition): FlattenedInfixChain = {
    val parts = getParts(pos)
    val selfPart = assoc match {
      case Assoc.Left  => parts.left
      case Assoc.Right => parts.right
    }
    val otherPart = assoc match {
      case Assoc.Left  => InfixChainPart(parts.operator, parts.right)
      case Assoc.Right => InfixChainPart(parts.operator, parts.left)
    }

    val selfSubtreeAsInfix = selfPart.astOpt.flatMap(GeneralizedInfix(_))
    val selfSubtreeFlattened = selfSubtreeAsInfix match {
      case Some(selfInfix) if selfInfix.name == name =>
        selfInfix.flattenInfix(selfPart.span.begin)
      case _ =>
        FlattenedInfixChain(selfPart, Seq())
    }

    selfSubtreeFlattened.copy(parts = otherPart +: selfSubtreeFlattened.parts)
  }
}
object GeneralizedInfix {

  /** Tries describing AST node as infix expression. */
  def apply(ast: AST): Option[GeneralizedInfix] = ast match {
    case AST.App.Infix.any(ast) =>
      Some(
        GeneralizedInfix(
          Some(ast.larg),
          ast.loff,
          ast.opr,
          ast.roff,
          Some(ast.rarg)
        )
      )
    case AST.App.Section.Left.any(ast) =>
      Some(GeneralizedInfix(Some(ast.arg), ast.off, ast.opr, 0, None))
    case AST.App.Section.Right.any(ast) =>
      Some(GeneralizedInfix(None, 0, ast.opr, ast.off, Some(ast.arg)))
    case AST.App.Section.Sides(opr) =>
      Some(GeneralizedInfix(None, 0, opr, 0, None))
    case _ => None
  }
}
