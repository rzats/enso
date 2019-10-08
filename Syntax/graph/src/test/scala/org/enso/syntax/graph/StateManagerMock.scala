package org.enso.syntax.graph

import org.enso.syntax.graph.API.Module
import org.enso.syntax.graph.SessionManager.Metadata
import org.enso.syntax.text.AST

/** Mock project state - contains a single module named `Main` with given
  * program body text.
  *
  * Initially no metadata is present, however whatever is set, will be visible
  * afterwards.
  */
final case class StateManagerMock(var program: String) extends StateManager {

  val module = new ModuleManager {
    var ast: AST.Module = ParserUtils.parse(program)

    override def list: Seq[Module.ID] =
      Seq(StateManagerMock.mainModule)

    override def apply(module: Module.ID): AST.Module =
      if (module == StateManagerMock.mainModule) ast
      else throw StateManager.ModuleNotFoundException(module)

    override def update(module: Module.ID, ast: AST.Module): Unit = {
      if (module != StateManagerMock.mainModule)
        throw new Exception(s"no such module: $module")

      program  = ast.show()
      this.ast = ast
      println(s"New AST for module $module: $ast")
      println(s"New Program text for module $module:\n$program")
    }
  }

  val metadata = new MetadataManager {
    var metadata: Map[(Module.ID, AST.ID), Metadata] = Map()

    override def apply(loc: Module.ID, id: AST.ID): Option[Metadata] =
      metadata.get(loc -> id)

    override def update(loc: Module.ID, id: AST.ID, data: Metadata): Unit =
      metadata = metadata + (loc -> id -> data)

    override def remove(module: Module.ID, id: AST.ID): Unit =
      metadata = metadata - (module -> id)
  }
}

object StateManagerMock {
  val mainModule: Module.ID = Module.Name("Main")
}
