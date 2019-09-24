package org.enso.syntax.graph

import org.enso.data.List1
import org.enso.data.List1.List1_ops
import org.enso.syntax.graph.API.Flag
import org.enso.syntax.graph.API.Module
import org.enso.syntax.graph.API.Node
import org.enso.syntax.graph.API.Port
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.App.Infix
import org.enso.syntax.text.AST.Block.Line
import org.enso.syntax.text.AST.Block.OptLine
import org.enso.syntax.text.AST.Import
import org.enso.syntax.text.AST.UnapplyByType
import org.enso.syntax.text.ast.opr.Assoc
import org.enso.syntax.text.ast.Repr._

import scala.collection.GenTraversableOnce

/** Operators with special meaning that we support.
  * // TODO consider where is the best place for this
  */
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

  /** A helper type to abstract away differences between proper infix operator
    * application and ones with missing argument (section apps). */
  final case class GeneralizedInfix(
    leftArgAst: Option[AST],
    leftSpace: Int,
    operatorAst: AST.Opr,
    rightSpace: Int,
    rightArgAst: Option[AST]
  ) {
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

    /** Converts nested operator applications into a flat list of all operands.
      *
      */
    def flattenInfix(
      pos: TextPosition
    ): Seq[ExpressionPartOrPoint] = {
      def flatten(child: ExpressionPartOrPoint): Seq[ExpressionPartOrPoint] =
        child.astOpt match {
          case Some(ast) if sameOperatorAsIn(ast) =>
            ast.flattenInfix(child.span.begin)
          case _ =>
            Seq(child)
        }

      val parts = getParts(pos)
      Assoc.of(operatorAst.name) match {
        case Assoc.Left  => flatten(parts.left) :+ parts.operator :+ parts.right
        case Assoc.Right => parts.left +: parts.operator +: flatten(parts.right)
      }
    }
  }
  object GeneralizedInfix {
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
        Some(
          GeneralizedInfix(Some(ast.arg), ast.off, ast.opr, 0, None)
        )
      case AST.App.Section.Right.any(ast) =>
        Some(GeneralizedInfix(None, 0, ast.opr, ast.off, Some(ast.arg)))
      case AST.App.Section.Sides(opr) =>
        Some(GeneralizedInfix(None, 0, opr, 0, None))
      case _ =>
        None
    }
  }

  implicit class Ast_ops(ast: AST) {
    def requireID: AST.ID =
      ast.id.getOrElse(throw MissingIdException(ast))

    def as[T: UnapplyByType]: Option[T] = UnapplyByType[T].unapply(ast)
    def is[T: UnapplyByType]: Boolean =
      UnapplyByType[T].unapply(ast).isDefined

    def toImport: Option[AST.Import] =
      // FIXME [mwu] I doubt we can use here resolved macro
      as[Import].orElse(as[AST.Macro.Match].flatMap(_.resolved.toImport))

    def toAssignment: Option[AST.App.Infix] =
      ast.as[AST.App.Infix].filter(_.opr.isAssignment)

    def toVarName: Option[String] = ast.as[AST.Var].map(_.name)

    def doesImport(module: Module.Name): Boolean = {
      ast.toImport.exists(_.path sameTarget module)
    }

    def flattenPrefix(
      pos: TextPosition,
      ast: AST.App.Prefix
    ): Seq[(TextPosition, AST)] = {
      val init = ast.fn match {
        case AST.App.Prefix.any(lhsApp) => flattenPrefix(pos, lhsApp)
        case nonAppAst                  => Seq(pos -> nonAppAst)
      }
      val rhsPos = pos + ast.fn.repr.span + ast.off
      val last   = rhsPos -> ast.arg
      init :+ last
    }

    def usedInfixOperator: Option[AST.Opr] =
      GeneralizedInfix(ast).map(_.operatorAst)

    def isInfixOperatorUsage: Boolean =
      ast.usedInfixOperator.nonEmpty

    def isInfixOperatorUsage(oprName: String): Boolean =
      ast.usedInfixOperator.exists(_.name == oprName)

    def flattenInfix(
      pos: TextPosition
    ): Seq[ExpressionPartOrPoint] = {
      GeneralizedInfix(ast) match {
        case None =>
          Seq(ExpressionPart(pos, ast))
        case Some(info) =>
          info.flattenInfix(pos)
      }
    }

    def asNode: Option[Node.Info] = {
      val (lhs, rhs) = ast.toAssignment match {
        case Some(Infix(l, _, r)) => (Some(l), r)
        case None                 => (None, ast)
      }

      // FIXME: provisional rule to not generate nodes from imports
      if (rhs.toImport.nonEmpty)
        return None

      val assignment = lhs.map(AssignmentInfo(_, rhs))

      // definition with inputs is a function
      // and a function is not a node
      if (assignment.exists(_.arguments.nonEmpty))
        return None

      val id = ast.requireID
      // TODO consider whether indexing should be shared for a line
      //   or separate for left and right sides of assignment
      val spanTree = SpanTree(rhs, TextPosition.Start)

      val output           = lhs.map(SpanTree(_, TextPosition.Start))
      val flags: Set[Flag] = Set.empty // TODO
      val stats            = None
      val metadata         = null // TODO

      val node = Node.Info(id, spanTree, output, flags, stats, metadata)
      Some(node)
    }

    // TODO replace using span tree machinery
    def flattenApps: List1[AST] = ast match {
      // TODO: provisionally deal with macros resolving to Group, like parens,
      //       as if code was directly grouped
      case AST.Macro.Match(_, _, group @ AST.Group(body)) =>
        body match {
          case Some(groupedAst) => groupedAst.flattenApps
          case _                => List1(group)
        }
      case AST.App.Prefix(lhs, rhs) => lhs.flattenApps :+ rhs
      case nonAppAst                => List1(nonAppAst)
    }

    def asPort: Port.Info = ast match {
      case _ => Port.Empty
    }
  }

  // FIXME: is this safe if module name is List1 ?
  implicit class ModuleName_ops(moduleName: API.Module.Name) {
    def nameAsString(): String = moduleName.toList.map(_.name).mkString(".")
    def sameTarget(rhs: API.Module.Name): Boolean =
      moduleName.map(_.shape) == rhs.map(_.shape)
  }

  implicit class Module_ops(module: AST.Module) {
    //def importedModules: List[Module.Name] = module.imports.map(_.path)
    def imports: List[Import]              = module.flatTraverse(_.toImport)
    def lineIndexOf(ast: AST): Option[Int] = lineIndexWhere(_ == ast).map(_._2)
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
      val moduleIsEmpty = module.lines.size == 1 && module.lines.head.elem.isEmpty
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
            case None        => l +: go(ls)
            case Some(lines) => lines ++ ls
          }
      }
      AST.Module(List1(go(module.lines.toList)).get)
    }
  }
}
