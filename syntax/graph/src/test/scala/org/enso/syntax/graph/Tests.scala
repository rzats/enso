package org.enso.syntax.graph

import org.enso.data.List1
import org.enso.syntax.graph.CommonAPI.Module.Id
import org.enso.syntax.text.AST
import org.enso.syntax.graph.CommonAPI._
import org.enso.syntax.text.AST.Cons
import org.scalatest._
import org.scalatest.Matchers._

import scala.reflect.ClassTag

/** Mock project state - contains a single module named `Main` with given body.
  */
final case class StateManagerMock(var program: String) extends StateManager {
  var ast: AST.Module = AstUtils.parse(program)

  override def availableModules(): Seq[Module.Id] =
    Seq(StateManagerMock.mainModule)
  override def getModuleAst(module: Id): AST.Module =
    AstUtils.parse(program)
  override def setModuleAst(module: Id, ast: AST.Module): Unit = {
    if (module != StateManagerMock.mainModule)
      throw new Exception(s"no such module: $module")

    this.program = ast.show()
    this.ast     = ast
    println(s"New AST for module $module: $ast")
    println(s"New Program text for module $module:\n$program")
  }
}

final case class NotificationConsumerMock() extends NotificationConsumer {
  override def send(notification: API.Notification): Unit = {
    println(s"Notification sent: $notification")
  }
}

object StateManagerMock {
  val mainModule: Module.Id = List1(Cons("Main"))
}

class Tests extends FunSuite with org.scalatest.Matchers {
  val mockModule = StateManagerMock.mainModule

  def withDR[R](
    program: String,
    f: DoubleRepresentation => R
  ): (R, AST.Module) = {
    val state                = StateManagerMock(program)
    val notificationConsumer = NotificationConsumerMock()
    val result               = f(DoubleRepresentation(state, notificationConsumer))
    (result, state.ast)
  }

  def checkThatTransforms[R](
    initialProgram: String,
    expectedFinalProgram: String,
    action: DoubleRepresentation => R
  ): R = {
    val (result, finalAst) = withDR(initialProgram, action)
    val actualFinalProgram = finalAst.show()
    actualFinalProgram should be(expectedFinalProgram)
    result
  }
  def expectTransformationError[E: ClassTag](
    initialProgram: String,
    action: DoubleRepresentation => Unit
  ): Unit = {
    an[E] should be thrownBy { withDR(initialProgram, action) }
  }

  test("adding first import") {
    checkThatTransforms(
      "",
      "import Foo.Baz",
      _.importModule(mockModule, Module.Name("Foo.Baz"))
    )
  }
  test("adding second import") {
    checkThatTransforms(
      "import Foo.Bar",
      "import Foo.Bar\nimport Foo.Baz",
      _.importModule(mockModule, Module.Name("Foo.Baz"))
    )
  }
  test("adding import when there is empty line and definition") {
    checkThatTransforms(
      """import Foo
        |
        |import Foo.Bar
        |
        |add x y = x + y""".stripMargin,
      """import Foo
        |
        |import Foo.Bar
        |import Foo.Baz
        |
        |add x y = x + y""".stripMargin,
      _.importModule(mockModule, Module.Name("Foo.Baz"))
    )
  }
  test("adding duplicated import") {
    expectTransformationError[API.ImportAlreadyExistsException](
      "import Foo.Bar",
      _.importModule(mockModule, Module.Name("Foo.Bar"))
    )
  }
  test("foo") {
    AstUtils.experimental()
//    val ast = AstUtils.parse("""import Foo
//                               |
//                               |import Foo.Bar
//                               |
//                               |""".stripMargin)
//    println(ast)
  }

}
