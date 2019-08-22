package org.enso.syntax.graph

import java.util.UUID

import org.enso.syntax.graph.API._
import org.enso.data.List1._
import org.enso.flexer.Parser.Result
import org.enso.flexer.Reader
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Import
import org.enso.syntax.text.Parser
import org.enso.syntax.graph.CommonAPI.Module
import org.enso.syntax.text.AST.App.Infix
import org.enso.syntax.text.AST.Block.Line

import scala.reflect.ClassTag

object Extensions {

  def downcast[T: ClassTag](ast: AST): Option[T] = {
    val tag = implicitly[ClassTag[T]]
    ast match {
      case AST.Marked(_, nested) => downcast(nested)
      case tag(t)                => Some(t)
      case _                     => None
    }
  }

  val DDD = downcast[AST.App.Infix](AST.Blank)

  /////////////////////

  trait FromAST[T] {
    def fromAST(ast: AST): Option[T]
  }

  implicit val astToInfix: FromAST[Infix] = {
    // TODO: do we somewhere named constant for "=" ?
    case AST.Marked(_, nested)       => astToInfix.fromAST(nested)
    case i @ Infix(_, AST.Opr(_), _) => Some(i)
    case _                           => None
  }

  implicit class Ast_ops(ast: AST) {

    def as[T: FromAST]: Option[T] =
      implicitly[FromAST[T]].fromAST(ast)

    def toAssignment: Option[Infix] = ast match {
      // TODO: do we somewhere named constant for "=" ?
      case AST.Marked(_, nested)         => nested.toAssignment
      case i @ Infix(_, AST.Opr("="), _) => Some(i)
      case _                             => None
    }
    def toImport: Option[Import] = ast match {
      case AST.Marked(_, nested) => nested.toImport
      case i @ Import(_)         => Some(i)
      case _                     => None
    }
    def asVar: Option[AST.Var] = ast match {
      case AST.Marked(_, nested) => nested.asVar
      case v @ AST.Var(_)        => Some(v)
      case _                     => None
    }
    def asName: Option[String] = ast.asVar.map(_.name)

    def requiresId: Boolean = {
      ast match {
        case _: AST.Var       => true
        case _: AST.App       => true
        case _: AST.App.Left  => true
        case _: AST.App.Right => true
        case _: AST.App.Infix => true
        case _: AST.Number    => true
        case _                => false
      }
    }

    def imports(module: Module.Name): Boolean = {
      ast.toImport.exists(_.path == module)
    }

    def definitionAst: Option[AST.Marked] = ast match {
      case _: Import          => None
      case marked: AST.Marked => Some(marked)
      case other =>
        println(s"warning unmarked perhaps definition: $other")
        None
    }

    def asNode: Option[Node.Info] =
      ast.definitionAst.flatMap {
        case AST.Marked(marker, defAst) =>
          val (lhs, rhs) = defAst.toAssignment match {
            case Some(Infix(l, AST.Opr("="), r)) => (Some(l), r)
            case None                            => (None, defAst)
          }

          val definition = lhs.map(DefInfo(_, rhs))

          // definition with inputs is a function
          // and a function is not a node
          if (definition.exists(_.inputAsts.nonEmpty))
            return None

          val id               = marker.id
          val spanTree         = null
          val expr             = Expr(rhs.show(), spanTree)
          val inputs           = Seq()
          val outputName       = definition.flatMap(_.name)
          val output           = Port.Info(None, outputName, Seq())
          val flags: Set[Flag] = Set.empty
          val stats            = null
          val metadata         = null

          val node = Node.Info(id, expr, inputs, output, flags, stats, metadata)
          Some(node)
      }

    def flattenApps: Seq[AST] = ast match {
      case AST.App(lhs, rhs) => lhs.flattenApps :+ rhs
      case nonAppAst         => Seq(nonAppAst)
    }
  }

  implicit class Line_ops(line: Line) {
    def asImport: Option[Import] = line.elem.flatMap(_.toImport)
    def imports(module: Module.Name): Boolean =
      line.elem.exists(_.imports(module))
    def asDefinition: Option[Infix]     = line.elem.flatMap(_.toAssignment)
    def asNode:       Option[Node.Info] = line.elem.flatMap(_.asNode)
  }

  implicit class Module_ops(module: AST.Module) {
    def importedModules: List[Module.Name] = module.imports.map(_.path)
    def imports:         List[Import]      = module.flatTraverse(_.toImport)
    def lineIndexOf(ast: AST): Option[Int] = {
      module.lines.indexWhere(_.elem.contains(ast))
    }
    def lineIndexWhere(p: AST => Boolean): Option[Int] = {
      module.lines.indexWhere(_.elem.exists(p))
    }
  }
}

// TODO ugly
import Extensions._

case class DefInfo(lhs: AST, rhs: AST) {
  val lhsParts: Seq[AST]              = lhs.flattenApps
  val name: Option[String]            = lhsParts.headOption.flatMap(_.asName)
  val inputAsts: Seq[AST]             = lhsParts.tail
  val inputNames: Seq[Option[String]] = inputAsts.map(_.asName)
}
object DefInfo {
  def apply(ast: AST.App.Infix): Option[DefInfo] =
    if (ast.opr.name == "=") Some(DefInfo(ast.larg, ast.rarg))
    else None
  def apply(ast: AST): Option[DefInfo] =
    ast.toAssignment.flatMap(DefInfo(_))
}

object AstUtils {
  def expectAst(result: Parser.Result[AST.Module]): AST.Module = {
    result match {
      case Result(_, Result.Success(ret)) => ret
      case _ =>
        throw new Exception("Parsing failed: " + result.toString)
    }
  }
  def prettyPrint(ast: AST.Module): Unit = {
    println("------")
    println(org.enso.syntax.text.Main.pretty(ast.toString))
    println("------")
    println(ast.show())
  }
  def parse(program: String, markers: Parser.Markers = Seq()): AST.Module = {
    val parser = new Parser()
    val result = parser.run(new Reader(program), markers)
    val ast    = expectAst(result)
    parser.resolveMacros(ast)
  }
}

final case class DoubleRepresentation(
  state: StateManager,
  notifier: NotificationConsumer
) extends GraphAPI {
  import CommonAPI._
  import API._

  def getGraph(loc: API.Definition.Graph.Location): Definition.Graph.Info = ???
  def getGraph(loc: Module.Graph.Location): Module.Graph.Info = {
    val ast   = state.getModule(loc.module)
    val nodes = ast.flatTraverse(_.asNode)
    Module.Graph.Info(nodes, Seq())
  }

  def getDefinitions(loc: Module.Location): List[Definition.Info] = ???
  def addNode(
    context: Node.Context,
    metadata: Node.Metadata,
    expr: String
  ): Node.Id = ???
  def setMetadata(node: Node.Location, newMetadata: Node.Metadata) = ???
  def enableFlag(node: Node.Location, flag: Flag)                  = ???
  def disableFlag(node: Node.Location, flag: Flag)                 = ???
  def setExpression(node: Node.Location, expression: String)       = ???
  def removeNode(node: Node.Location)                              = ???
  def extractToFunction(
    context: Node.Context,
    node: Set[Node.Id]
  ): Definition.Id = ???
  def setPortName(port: Graph.Port.Location, name: String)                 = ???
  def addPort(port: Graph.Port.Location, name: String, tp: Option[Type])   = ???
  def removePort(port: Graph.Port.Location)                                = ???
  def addConnection(graph: Port.Context, from: Output.Id, to: Input.Id)    = ???
  def removeConnection(graph: Port.Context, from: Output.Id, to: Input.Id) = ???

  override def importedModules(module: Module.Location): Seq[Module.Name] = {
    val ast = state.getModule(module)
    ast.imports.map(_.path)
  }

  override def importModule(context: Module.Id, importee: Module.Name): Unit = {
    val ast            = state.getModule(context)
    val currentImports = ast.imports
    if (currentImports.exists(_ imports importee))
      throw ImportAlreadyExistsException(importee)

    // the new import shall be placed in the line after the last import in the
    // module or at the beginning of the file
    val lineToPlaceImport = currentImports.lastOption
      .flatMap(ast.lineIndexOf(_).map(_ + 1))
      .getOrElse(0)

    val newAst = ast.insert(lineToPlaceImport, Import(importee))
    state.setModule(context, newAst)
    notifier.send(API.Notification.Invalidate.Module(context))
  }

  override def removeImport(
    context: Module.Id,
    importToRemove: Module.Name
  ): Unit = {
    val ast = state.getModule(context)
    val lineIndex = ast.lineIndexWhere(_ imports importToRemove) match {
      case Some(index) => index
      case None        => throw NoSuchImportException(importToRemove)
    }

    val newAst = ast.removeAt(lineIndex)
    state.setModule(context, newAst)
    notifier.send(API.Notification.Invalidate.Module(context))
  }
}
