package org.enso.syntax.graph

import org.enso.flexer.Reader
import AstOps._
import org.enso.syntax.graph.API._
import org.enso.syntax.text.AST.Import
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.App.Infix
import org.enso.syntax.text.Parser
import org.enso.syntax.text.ast.Repr._

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
    ast
  }
  def preprocess(program: String): String = new Reader(program).toString()
}

final case class DoubleRepresentation(
  state: StateManager,
  notifier: NotificationSink
) extends GraphAPI
    with TextAPI {

  def getText(loc: Module.Location): String = state.module(loc).show()

  def insertText(
    loc: Module.Location,
    at: TextPosition,
    text: String
  ): Unit = {
    state.module(loc) = state.module(loc).replaceAt(at) { (pos, line) =>
      val (prefix, suffix) = line.show.splitAt(pos.index + at.index)
      val result           = Parser().run(new Reader(prefix + text + suffix))
      result.lines.toList
    }
    notifier.notify(TextAPI.Notification.Inserted(loc, at, text))
    notifier.notify(GraphAPI.Notification.Invalidate.Module(loc))
  }

  def eraseText(loc: Module.Location, span: TextSpan): Unit = {
    state.module(loc) = state.module(loc).replaceAt(span.begin) { (pos, line) =>
      val (line1, line2) = line.show.splitAt(pos.index + span.begin.index)
      val result =
        Parser().run(new Reader(line1 + line2.drop(span.length.value)))
      result.lines.toList
    }
    notifier.notify(TextAPI.Notification.Erased(loc, span))
    notifier.notify(GraphAPI.Notification.Invalidate.Module(loc))
  }

  def copyText(loc: Module.Location, span: TextSpan): String = {
    var text = ""
    state.module(loc) = state.module(loc).replaceAt(span.begin) { (pos, line) =>
      val start = pos.index + span.begin.index
      text = line.show.substring(start, start + span.length.value)
      List(line)
    }
    text
  }

  def pasteText(
    module: Module.Location,
    at: TextPosition,
    clipboard: String
  ): Unit =
    insertText(module, at, clipboard)

  def getGraph(
    loc: API.Definition.Graph.Location
  ): Definition.Graph.Description = ???
  def getGraph(loc: Module.Graph.Location): Module.Graph.Description = {
    val ast   = state.module(loc.module)
    val nodes = ast.flatTraverse(describeNode(loc.module, _))
    Module.Graph.Description(nodes, Seq())
  }

  def getDefinitions(loc: Module.Location): List[Definition.Description] = ???
  def addNode(
    context: Node.Context,
    metadata: SessionManager.Metadata,
    expr: String
  ): Node.ID = ???
  def setMetadata(
    node: Node.Location,
    newMetadata: SessionManager.Metadata
  ): Unit = {
    state.metadata(node.context.module, node.node, newMetadata)
    val notification =
      GraphAPI.Notification.Node.Changed.Metadata(node, newMetadata)
    notifier.notify(notification)
  }
  def setExpression(node: Node.Location, expression: String) = {
    val loc = state.module(node.context.module)
    var found = false
    state.module(loc) = state.module(loc).replaceAt(node.node) { line =>
      line.elem.get match {
        case Infix(lhs, m, _) =>
          found = true
          val term = Parser().run(new Reader(expression))
          List(line.copy(elem = Some(Infix(lhs, m, term)))) //FIXME keep offsets
        case _ => List(line)
      }

    }
    if (!found) addNode(node.context, SessionManager.Metadata(), expression)
  }
  def removeNode(node: Node.Location) = {
    val loc = state.module(node.context.module)
    state.module(loc) = state.module(loc).replaceAt(node.node)(_ => Nil)
  }
  def extractToFunction(
    context: Node.Context,
    node: Set[Node.ID]
  ): Definition.ID                                                       = ???
  def setPortName(port: Graph.Port.Location, name: String)               = ???
  def addPort(port: Graph.Port.Location, name: String, tp: Option[Type]) = ???
  def removePort(port: Graph.Port.Location)                              = ???
  def addConnection(graph: Port.Context, from: Output.ID, to: Input.ID) =
    ???
  def removeConnection(graph: Port.Context, from: Output.ID, to: Input.ID) =
    ???

  override def importedModules(module: Module.Location): Seq[Module.Name] = {
    val ast     = state.module(module)
    val imports = ast.imports
    val paths   = imports.map(_.path)
    paths
  }

  override def importModule(loc: Module.ID, importee: Module.Name): Unit = {
    val module         = state.module(loc)
    val currentImports = module.imports
    if (currentImports.exists(_ doesImport importee))
      throw ImportAlreadyExistsException(importee)

    val lastImportPosition = currentImports.lastOption.flatMap(
      lastImport => module.lineIndexWhere(_.doesImport(lastImport.path))
    )
    val lineToPlaceImport = lastImportPosition match {
      case Some((_, lastImportLineNumber)) => lastImportLineNumber + 1
      case None                            => 0
    }

    state.module(loc) = module.insert(lineToPlaceImport, Import(importee))
    notifier.notify(GraphAPI.Notification.Invalidate.Module(loc))

    val text   = AST.Import(importee).show()
    val offset = TextPosition(module.lineOffset(lineToPlaceImport))
    notifier.notify(TextAPI.Notification.Inserted(loc, offset, text))
  }

  override def removeImport(
    loc: Module.ID,
    importToRemove: Module.Name
  ): Unit = {
    val module = state.module(loc)
    val (line, lineIx) =
      module.lineIndexWhere(_ doesImport importToRemove) match {
        case Some(index) => index
        case None        => throw NoSuchImportException(importToRemove)
      }

    state.module(loc) = module.removeAt(lineIx)
    notifier.notify(GraphAPI.Notification.Invalidate.Module(loc))

    val offset = TextPosition(module.lineOffset(lineIx))
    val span   = TextSpan(offset, TextLength(line.span))
    notifier.notify(TextAPI.Notification.Erased(loc, span))
  }

  /////////////////
  //// Helpers ////
  /////////////////

  def describeNode(
    module: Module.Location,
    ast: AST
  ): Option[API.Node.Description] = {
    val (lhs, rhs) = ast.toAssignment match {
      case Some(Infix(l, _, r)) => (Some(l), r)
      case None                 => (None, ast)
    }

    // FIXME: [mwu] provisional rule to not generate nodes from imports
    if (rhs.toImport.nonEmpty)
      return None

    // ignore definitions, i.e. assignments with arguments on left side
    if (lhs.exists(_.is[AST.App.Prefix]))
      return None

    val id       = ast.unsafeID
    val spanTree = SpanTree(rhs, TextPosition.Start)
    val output   = lhs.map(SpanTree(_, TextPosition.Start))
    val metadata = state.metadata(module, id)
    val node     = Node.Description(id, spanTree, output, metadata)
    Some(node)
  }
}
