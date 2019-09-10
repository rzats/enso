package org.enso.syntax.graph

import org.enso.data.List1
import org.enso.syntax.graph.API._
import org.enso.data.List1._
import org.enso.flexer.Reader
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Macro.Match
import org.enso.syntax.text.AST.Import
import org.enso.syntax.text.Parser
import AstOps._

case class MissingIdException(ast: AST) extends Exception {
  override def getMessage: String =
    s"missing id for node with expression `${ast.show()}`"
}

case class AssignmentInfo(lhs: AST, rhs: AST) {
  val lhsParts: List1[AST]            = lhs.flattenApps
  val name: Option[String]            = lhsParts.head.varName
  val inputAsts: Seq[AST]             = lhsParts.tail
  val inputNames: Seq[Option[String]] = inputAsts.map(_.varName)
}

object ParserUtils {
  def prettyPrint(ast: AST.Module): Unit = {
    println("------")
    println(org.enso.syntax.text.Main.pretty(ast.toString))
    println("------")
    println(ast.show())
  }
  def parse(program: String): AST.Module = {
    val parser = new Parser()
    val ast    = parser.run(new Reader(program))

//    var counter = 0
//    ast.map(astNode => {
//      // unmarked node asts should get their markers
//      if (astNode.requiresId) {
//        val markedAst = AST.Marked(AST.Marker(counter), astNode)
//        counter += 1
//        markedAst
//      } else
//        astNode
//    })
    ast
  }
}

final case class DoubleRepresentation(
  state: StateManager,
  notifier: NotificationSink
) extends GraphAPI {

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
  def setPortName(port: Graph.Port.Location, name: String)               = ???
  def addPort(port: Graph.Port.Location, name: String, tp: Option[Type]) = ???
  def removePort(port: Graph.Port.Location)                              = ???
  def addConnection(graph: Port.Context, from: Output.Id, to: Input.Id) =
    ???
  def removeConnection(graph: Port.Context, from: Output.Id, to: Input.Id) =
    ???

  override def importedModules(module: Module.Location): Seq[Module.Name] = {
    val ast     = state.getModule(module)
    val imports = ast.imports
    val paths   = imports.map(_.path)
    paths
  }

  override def importModule(context: Module.Id, importee: Module.Name): Unit = {
    val module         = state.getModule(context)
    val currentImports = module.imports
    if (currentImports.exists(_ imports importee))
      throw ImportAlreadyExistsException(importee)

    // TODO perhaps here zippers could be useful?

    val lastImportPosition = currentImports.lastOption.flatMap(
      lastImport => module.lineIndexWhere(_.imports(lastImport.path))
    )
    val lineToPlaceImport = lastImportPosition match {
      case Some(lastImportLineNumber) => lastImportLineNumber + 1
      case None                       => 0
    }

    val newAst = module.insert(lineToPlaceImport, Import(importee))
    state.setModule(context, newAst)
    notifier.retrieve(API.Notification.Invalidate.Module(context))
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
    notifier.retrieve(API.Notification.Invalidate.Module(context))
  }
}
