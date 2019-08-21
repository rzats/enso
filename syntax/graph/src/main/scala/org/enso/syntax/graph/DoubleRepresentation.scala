package org.enso.syntax.graph

import org.enso.syntax.graph.API._
import org.enso.data.List1
import org.enso.flexer.Parser.Result
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Import
import org.enso.syntax.text.Parser
import org.enso.syntax.graph.CommonAPI.Module
import org.enso.syntax.text.AST.App.Infix
import org.enso.syntax.text.AST.Block.Line

object LineHelpers {
  def asImport(line: Line): Option[Import] = line.elem match {
    case Some(i @ Import(_)) => Some(i)
    case _                   => None
  }
  def asDefinition(line: Line): Option[Infix] = line.elem match {
    case Some(i @ Infix(_, AST.Opr("="), _)) => Some(i)
    case _                                   => None
  }
}

object AstUtils {
  def expectAst(result: Parser.Result[AST.Module]): AST.Module = {
    result match {
      case Result(_, Result.Success(ret)) => ret
      case _ =>
        throw new Exception("Parsing failed: " + result.toString)
    }
  }

  def lineAsts(ast: AST.Module): List[AST] =
    ast.lines.toList.flatMap(_.elem)

  def collectImports(ast: AST.Module): List[AST.Import] = {
    ast.lines.toList.flatMap(LineHelpers asImport _)
  }

  def prettyPrint(ast: AST.Module): Unit = {
    println("------")
    println(org.enso.syntax.text.Main.pretty(ast.toString))
    println("------")
    println(ast.show())
  }

  def experimental(): Unit = {
    val parser  = new Parser()
    val program = """import Foo
                    |
                    |import Foo.Bar
                    |
                    |""".stripMargin
    val result  = parser.run(program)
    println(result)
  }

  def parse(program: String, markers: Parser.Markers = Seq()): AST.Module = {
    val parser = new Parser()
    val result = parser.run(program, markers)
    val ast    = expectAst(result)
    parser.resolveMacros(ast)
  }

  def importedModules(ast: AST.Module): List[Module.Id] = {
    val imports: List[AST.Import] = collectImports(ast)
    imports.map(_.path)
  }

  def asDefinition(): PartialFunction[AST, (AST, AST)] = {
    case Infix(l, AST.Opr("="), r) => (l, r)
  }

  def asVarName: PartialFunction[AST, String] = {
    case AST.Var(name) => name
  }

  def flattenDefinitionLhs(ast: AST): List1[AST] = {
    ast match {
      case AST.App(lhs, rhs) => flattenDefinitionLhs(lhs) :+ rhs
      case nonAppAst         => List1(nonAppAst)
    }
  }

  def getDefinitionName(ast: AST): String = {
    println(s"Looking for name in ${ast.show()}")
    ast match {
      case AST.Var(varname) => varname
      case AST.App(lhs, _)  => getDefinitionName(lhs)
      case sth =>
        throw new Exception(s"not supported AST element: $sth")
    }
  }

  case class Definition(lhs: AST, rhs: AST) {
    val lhsParts: List1[AST]    = flattenDefinitionLhs(lhs)
    val name: String            = asVarName(lhsParts.head)
    val inputAsts: Seq[AST]     = lhsParts.tail
    val inputNames: Seq[String] = inputAsts.map(asVarName)
  }

  def definitions(ast: AST.Module): Seq[Definition] = {
    lineAsts(ast).collect {
      case Infix(l, AST.Opr("="), r) => Definition(l, r)
    }
  }
}

final case class DoubleRepresentation(
  state: StateManager,
  notifier: NotificationConsumer
) extends GraphAPI {
  import CommonAPI._
  import API._

  def getGraph(loc: Graph.Location):        Graph.Info            = ???
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
    val ast     = state.getModuleAst(module)
    val imports = AstUtils.collectImports(ast)
    imports.map(_.path)
  }

  def importModule(context: Module.Id, importee: Module.Name): Unit = {
    val ast            = state.getModuleAst(context)
    val currentImports = AstUtils.collectImports(ast)
    if (currentImports.exists(i => i.path == importee))
      throw ImportAlreadyExistsException(importee)

    // the new import shall be placed in the line after the last import in the
    // module or at the beginning of the file
    val lineToPlaceImport = currentImports.lastOption match {
      case Some(lastImport) =>
        val lastIndex = ast.lines.toList.indexWhere(_.elem.contains(lastImport))
        lastIndex + 1
      case None =>
        0
    }
    val addedImportLine = Line(Import(importee))

    val newAst = ast.insert(lineToPlaceImport, addedImportLine)
    state.setModuleAst(context, newAst)
    notifier.send(API.Notification.Invalidate.Module(context))
  }
  def removeImport(moduleToNotBeImportedAnymore: Module.Name) = ???
}
