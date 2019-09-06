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

import scala.collection.GenTraversableOnce

object KnownOperators {
  val Assignment = AST.Opr("=")
  val Minus      = AST.Opr("-")
}

object AstOps {
  implicit class Opr_ops(opr: AST.Opr) {
    def isAssignment: Boolean = opr == KnownOperators.Assignment
  }

  case class GeneralizedInfixOperator(
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
  }
  object GeneralizedInfixOperator {
    def apply(ast: AST): Option[GeneralizedInfixOperator] = ast match {
      case AST.App.Infix.any(ast) =>
        Some(
          GeneralizedInfixOperator(
            Some(ast.larg),
            ast.loff,
            ast.opr,
            ast.roff,
            Some(ast.rarg)
          )
        )
      case AST.App.Section.Left.any(ast) =>
        Some(
          GeneralizedInfixOperator(Some(ast.arg), ast.off, ast.opr, 0, None)
        )
      case AST.App.Section.Right.any(ast) =>
        Some(GeneralizedInfixOperator(None, 0, ast.opr, ast.off, Some(ast.arg)))
      case AST.App.Section.Sides(opr) =>
        Some(GeneralizedInfixOperator(None, 0, opr, 0, None))
      case _ =>
        None
    }
  }

  implicit class Ast_ops(ast: AST) {
    def getId: AST.ID =
      ast.id.getOrElse(throw MissingIdException(ast))

    def as[T: UnapplyByType]: Option[T] = UnapplyByType[T].unapply(ast)

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
      GeneralizedInfixOperator(ast).map(_.operator)

    def isInfixOperatorUsage: Boolean =
      ast.usedInfixOperator.nonEmpty

    def isInfixOperatorUsage(oprName: String): Boolean =
      ast.usedInfixOperator.exists(_.name == oprName)

    def flattenInfix(
      pos: TextPosition,
      ast: AST
    ): Seq[(TextPosition, Option[AST])] = {
      println("flattening on " + ast.show())
      GeneralizedInfixOperator(ast) match {
        case None =>
          Seq(pos -> Some(ast))
        case Some(info @ GeneralizedInfixOperator(lhs, _, opr, _, rhs)) =>
          val init = lhs match {
            case Some(lhsAst) if lhsAst.isInfixOperatorUsage(opr.name) =>
              flattenInfix(pos, lhsAst)
            case _ =>
              Seq(pos -> lhs)
          }

          // TODO support right-associative operators
          val preLast = pos + info.oprPos      -> Some(info.operator)
          val last    = pos + info.rightArgPos -> info.rightArg
          init :+ preLast :+ last
      }
    }

    def spanTreeNode(pos: TextPosition): SpanTree = ast match {
      case AST.Opr.any(opr)          => SpanTree.Atom(pos, opr)
      case AST.Blank.any(_)          => SpanTree.Atom(pos, ast)
      case AST.Literal.Number.any(_) => SpanTree.Atom(pos, ast)
      case AST.Var.any(_)            => SpanTree.Atom(pos, ast)
      case AST.App.Prefix.any(app) =>
        val childrenAsts = flattenPrefix(pos, app)
        val childrenNodes = childrenAsts.map {
          case (childPos, childAst) =>
            childAst.spanTreeNode(childPos)
        }
        SpanTree.ApplicationChain(SpanTree.AstNodeInfo(pos, ast, childrenNodes))
      case ast if ast.isInfixOperatorUsage =>
        val childrenAsts = flattenInfix(pos, ast)
        val childrenNodes = childrenAsts.map {
          case (childPos, childAst) =>
            childAst
              .map(_.spanTreeNode(childPos))
              .getOrElse(SpanTree.EmptyEndpoint(childPos))
        }
        val info = SpanTree.AstNodeInfo(pos, ast, childrenNodes)
        SpanTree.OperatorChain(info, ast.usedInfixOperator.get) // FIXME
      case _ =>
        println("failed to generate span tree node for " + ast.show())
        println(ast.toString())
        null
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

      val inputAsts = rhs.groupTopInputs
      // TODO subports
      val inputs           = inputAsts.map(_.asPort)
      val outputName       = assignment.flatMap(_.name)
      val output           = Port.Info(None, outputName, Seq())
      val flags: Set[Flag] = Set.empty // TODO
      val stats            = None
      val metadata         = null // TODO

      val node = Node.Info(id, spanTree, inputs, output, flags, stats, metadata)
      Some(node)
    }

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

    def groupTopInputs: Seq[AST] = ast match {
      case AST.App.Prefix.any(ast)      => ast.flattenApps.tail
      case AST.App.Section.Sides.any(_) => Seq(AST.Blank(), AST.Blank())
      // TODO special case below for unary minus, until parser handles it
      case AST.App.Section.Right(KnownOperators.Minus, rhs) => Seq(rhs)
      case AST.App.Section.Right(_, rhs)                    => Seq(AST.Blank(), rhs)
      case AST.App.Section.Left(lhs, _)                     => Seq(lhs, AST.Blank())
      case AST.App.Infix(lhs, _, rhs)                       => Seq(lhs, rhs)
      //      case _: AST.Var                 => Seq()
      //      case _: AST.Literal             => Seq()
      //      case _: AST.Number              => Seq()
      case _ => Seq()
    }

    def asPort: Port.Info = ast match {
      case _ => Port.Empty
    }
  }

  implicit class Line_ops(line: Line) {
    //    def asImport: Option[Import] = line.elem.flatMap(_.as[Import])
    //    def imports(module: Module.Name): Boolean =
    //      line.elem.exists(_.imports(module))
    // def asAssignment: Option[Infix]     = line.elem.flatMap(_.asAssignment)
    // def asNode: Option[Node.Info] = line.elem.flatMap(_.asNode)
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
