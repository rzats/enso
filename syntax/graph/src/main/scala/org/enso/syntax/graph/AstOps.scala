package org.enso.syntax.graph

import org.enso.data.List1
import org.enso.data.List1.List1_ops
import org.enso.syntax.graph.API.Flag
import org.enso.syntax.graph.API.Module
import org.enso.syntax.graph.API.Node
import org.enso.syntax.graph.API.Port
import org.enso.syntax.graph.API.TextPosition
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.App.Infix
import org.enso.syntax.text.AST.Block.Line
import org.enso.syntax.text.AST.Block.OptLine
import org.enso.syntax.text.AST.Import
import org.enso.syntax.text.AST.UnapplyByType
import org.enso.syntax.text.ast.opr.Assoc

import scala.collection.GenTraversableOnce

object KnownOperators {
  val Assignment = "="
  val Access     = "."
  val Minus      = "-"
}

object AstOps {
  implicit class Opr_ops(opr: AST.Opr) {
    def isAssignment: Boolean = opr.name == KnownOperators.Assignment
    def isAccess:     Boolean = opr.name == KnownOperators.Access
    def isMinus:      Boolean = opr.name == KnownOperators.Minus
  }

  case class ExpressionPart(pos: TextPosition, ast: Option[AST])
  object ExpressionPart {
    def apply(pos: TextPosition, ast: AST): ExpressionPart =
      ExpressionPart(pos, Some(ast))
  }

  /** A helper type to abstract away differences between infix operator applied
    * to a different number of arguments. */
  case class GeneralizedInfix(
    leftArg: Option[AST],
    leftSpace: Int,
    operator: AST.Opr,
    rightSpace: Int,
    rightArg: Option[AST]
  ) {
    def name:                   String = operator.name
    def span(ast: Option[AST]): Int    = ast.map(_.repr.span).getOrElse(0)

    def oprPos:      Int = span(leftArg) + leftSpace
    def rightArgPos: Int = oprPos + operator.repr.span + rightSpace
    def usesSameOperator(ast: Option[AST]): Boolean =
      ast.exists(_.isInfixOperatorUsage(operator.name))

    def parts(offset: TextPosition) = {
      (
        ExpressionPart(offset, leftArg),
        ExpressionPart(offset + oprPos, operator),
        ExpressionPart(offset + rightArgPos, rightArg)
      )
    }

    def flattenInfix(
      pos: TextPosition
    ): Seq[ExpressionPart] = {
      def flatten(child: ExpressionPart) =
        if (usesSameOperator(child.ast))
          child.ast.get.flattenInfix(child.pos)
        else
          Seq(child)

      parts(pos) match {
        case (lhs, opr, rhs) =>
          Assoc.of(operator.name) match {
            case Assoc.Left  => flatten(lhs) :+ opr :+ rhs
            case Assoc.Right => lhs +: opr +: flatten(rhs)
          }
        case _ => throw new Exception("impossible happened")
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
    def getId: AST.ID =
      ast.id.getOrElse(throw MissingIdException(ast))

    def as[T: UnapplyByType]: Option[T] = UnapplyByType[T].unapply(ast)
    def is[T: UnapplyByType]: Boolean =
      UnapplyByType[T].unapply(ast).isDefined

    // TODO: name should be more different from as[Import]
    def asImport: Option[AST.Import] =
      as[Import].orElse(as[AST.Macro.Match].flatMap(_.resolved.asImport))

    def asAssignment: Option[AST.App.Infix] =
      ast.as[AST.App.Infix].filter(_.opr.isAssignment)

    def varName: Option[String] = ast.as[AST.Var].map(_.name)

    def imports(module: Module.Name): Boolean = {
      ast.asImport.exists(_.path sameTarget module)
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
      GeneralizedInfix(ast).map(_.operator)

    def isInfixOperatorUsage: Boolean =
      ast.usedInfixOperator.nonEmpty

    def isInfixOperatorUsage(oprName: String): Boolean =
      ast.usedInfixOperator.exists(_.name == oprName)

    def flattenInfix(
      pos: TextPosition
    ): Seq[ExpressionPart] = {
      println("flattening on " + ast.show())
      GeneralizedInfix(ast) match {
        case None =>
          Seq(ExpressionPart(pos, ast))
        case Some(info) =>
          info.flattenInfix(pos)
      }
    }

    def spanTreeNode(pos: TextPosition): SpanTree = ast match {
      case AST.Opr.any(opr)          => SpanTree.AstLeaf(pos, opr)
      case AST.Blank.any(_)          => SpanTree.AstLeaf(pos, ast)
      case AST.Literal.Number.any(_) => SpanTree.AstLeaf(pos, ast)
      case AST.Var.any(_)            => SpanTree.AstLeaf(pos, ast)
      case AST.App.Prefix.any(app) =>
        val info         = SpanTree.AstNodeInfo(pos, ast)
        val childrenAsts = flattenPrefix(pos, app)
        val childrenNodes = childrenAsts.map {
          case (childPos, childAst) =>
            childAst.spanTreeNode(childPos)
        }
        childrenNodes match {
          case callee :: args =>
            callee match {
              // FIXME support non-atoms
              case calleeAtom: SpanTree.AstLeaf =>
                SpanTree.ApplicationChain(info, calleeAtom, args)
              case _ =>
                throw new Exception(
                  s"The application target $callee was not an atom"
                )
            }
          case _ =>
            // app prefix always has two children, flattening can only add more
            throw new Exception("impossible: failed to find application target")
        }

      case _ =>
        GeneralizedInfix(ast) match {
          case Some(info) =>
            val childrenAsts = info.flattenInfix(pos)
            val childrenNodes = childrenAsts.map { part =>
              part.ast
                .map(_.spanTreeNode(part.pos))
                .getOrElse(SpanTree.EmptyEndpoint(part.pos))
            }

            val nodeInfo = SpanTree.AstNodeInfo(pos, ast)

            val self = childrenNodes.headOption.getOrElse(
              throw new Exception(
                "internal error: infix with no children nodes"
              )
            )
            val calls = childrenNodes
              .drop(1)
              .sliding(2, 2)
              .map {
                case (opr: SpanTree.AstLeaf) :: (arg: SpanTree) :: Nil =>
                  (opr, arg)
              }
              .toSeq

            if (info.operator.isAccess)
              SpanTree.AccessorPath(nodeInfo, info.operator, self, calls)
            else
              SpanTree.OperatorChain(nodeInfo, info.operator, self, calls)
          case _ =>
            println("failed to generate span tree node for " + ast.show())
            println(ast.toString())
            null
        }
    }
    def spanTree: SpanTree = ast.spanTreeNode(TextPosition.Start)

    def asNode: Option[Node.Info] = {
      val (lhs, rhs) = ast.asAssignment match {
        case Some(Infix(l, _, r)) => (Some(l), r)
        case None                 => (None, ast)
      }

      val assignment = lhs.map(AssignmentInfo(_, rhs))

      // definition with inputs is a function
      // and a function is not a node
      if (assignment.exists(_.inputAsts.nonEmpty))
        return None

      val id       = ast.getId
      val spanTree = rhs.spanTree

      val output           = lhs.map(_.spanTree)
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

  implicit class ModuleName_ops(moduleName: API.Module.Name) {
    def nameAsString(): String = moduleName.toList.map(_.name).mkString(".")
    def sameTarget(rhs: API.Module.Name): Boolean =
      moduleName.map(_.unFix) == rhs.map(_.unFix)
  }

  implicit class Module_ops(module: AST.Module) {
    //def importedModules: List[Module.Name] = module.imports.map(_.path)
    def imports:               List[Import] = module.flatTraverse(_.asImport)
    def lineIndexOf(ast: AST): Option[Int]  = lineIndexWhere(_ == ast)
    def lineIndexWhere(p: AST => Boolean): Option[Int] = {
      module.lines.indexWhere(_.elem.exists(p))
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
