package org.enso.syntax.graph

import org.enso.data.List1
import org.enso.syntax.graph
import org.enso.syntax.graph.AstOps._
import org.enso.syntax.text.AST
import org.enso.syntax.graph.API._
import org.enso.syntax.text.AST.Cons
import org.scalatest._

import scala.meta.Tree
import scala.reflect.ClassTag

/** Mock project state - contains a single module named `Main` with given body.
  */
final case class StateManagerMock(var program: String) extends StateManager {
  var ast: AST.Module = ParserUtils.parse(program)

  override def availableModules(): Seq[Module.Id] =
    Seq(StateManagerMock.mainModule)

  override def getModule(module: Module.Id): AST.Module = {
    if (module == StateManagerMock.mainModule)
      ast
    else throw graph.StateManager.ModuleNotFoundException(module)
  }

  override def setModule(module: Module.Id, ast: AST.Module): Unit = {
    if (module != StateManagerMock.mainModule)
      throw new Exception(s"no such module: $module")

    this.program = ast.show()
    this.ast     = ast
    println(s"New AST for module $module: $ast")
    println(s"New Program text for module $module:\n$program")
  }
}

final case class NotificationSinkMock() extends NotificationSink {
  var notificationsReceived: Seq[API.Notification] = Seq()
  override def retrieve(notification: API.Notification): Unit = {
    println(s"Got notification: $notification")
    notificationsReceived = notificationsReceived :+ notification
  }
}

object StateManagerMock {
  val mainModule: Module.Id = List1(Cons("Main"))
}

object TestUtils {
  val InsertionMark                 = '⎀'
  val BeginSettableAndErasableBlock = '«'
  val EndSettableAndErasableBlock   = '»'
  val BeginSettableBlock            = '‹'
  val EndSettableBlock              = '›'

  case class ProgramTestCase(
    program: String,
    expectedInsertionPoints: Seq[TextPosition],
    expectedSettableParts: Seq[TextSpan],
    expectedErasableParts: Seq[TextSpan]
  )

  object ProgramTestCase {
    def apply(markedProgram: String): ProgramTestCase = {
      val markdownChars =
        Seq(
          InsertionMark,
          BeginSettableAndErasableBlock,
          EndSettableAndErasableBlock,
          BeginSettableBlock,
          EndSettableBlock
        )
      val program = markedProgram.filterNot(markdownChars.contains(_))

      var insertionPoints: Seq[TextPosition] = Seq()
      var settableSpans: Seq[TextSpan]       = Seq()
      var erasableSpans: Seq[TextSpan]       = Seq()

      var beginSetErasableBlock: Seq[TextPosition] = Seq()
      var beginSettableBlock: Seq[TextPosition]    = Seq()

      var index = TextPosition(0)
      markedProgram.foreach {
        case InsertionMark =>
          insertionPoints = insertionPoints :+ index
        case BeginSettableAndErasableBlock =>
          beginSetErasableBlock = beginSetErasableBlock :+ index
        case EndSettableAndErasableBlock =>
          settableSpans         = settableSpans :+ (beginSetErasableBlock.last <-> index)
          erasableSpans         = erasableSpans :+ (beginSetErasableBlock.last <-> index)
          beginSetErasableBlock = beginSetErasableBlock.dropRight(1)
        case BeginSettableBlock =>
          beginSettableBlock = beginSettableBlock :+ index
        case EndSettableBlock =>
          settableSpans      = settableSpans :+ (beginSettableBlock.last <-> index)
          beginSettableBlock = beginSettableBlock.dropRight(1)
        case _ =>
          index += 1
      }

      ProgramTestCase(program, insertionPoints, settableSpans, erasableSpans)
    }
  }
}

trait TestUtils extends org.scalatest.Matchers {
  type ProgramText = String

  val mockModule = StateManagerMock.mainModule

  def withDR[R](program: ProgramText)(
    f: DoubleRepresentation => R
  ): (R, AST.Module) = {
    val state                = StateManagerMock(program)
    val notificationConsumer = NotificationSinkMock()
    val result               = f(DoubleRepresentation(state, notificationConsumer))
    (result, state.ast)
  }

  def checkThatTransforms[R](
    initialProgram: String,
    expectedFinalProgram: String,
    action: DoubleRepresentation => R
  ): R = {
    val (result, finalAst) = withDR(initialProgram)(action)
    val actualFinalProgram = finalAst.show()
    actualFinalProgram should be(expectedFinalProgram.replace("\r\n", "\n"))
    result
  }
  def expectTransformationError[E: ClassTag](initialProgram: String)(
    action: DoubleRepresentation => Unit
  ): Unit = {
    an[E] should be thrownBy { withDR(initialProgram)(action) }
    ()
  }
  def checkModuleSingleNodeGraph[R](
    program: ProgramText
  )(action: API.Node.Info => R): R = {
    withDR(program) { dr =>
      val graph = dr.getGraph(Module.Graph.Location(mockModule))
      graph.nodes should have size 1
      graph.links should have size 0
      action(graph.nodes.head)
    }._1
  }
  def testSpanTreeFor[R](
    program: ProgramText
  )(action: SpanTree => R): R =
    checkModuleSingleNodeGraph(program) { node =>
      // make sure that span covers the whole expression
      val expectedSpan = TextSpan(TextPosition.Start, program.length)
      node.expr.span shouldEqual expectedSpan

      verifyTreeIndices(node.expr)
      action(node.expr)
    }

  def testSpanTreeMarkdown[R](
    markedProgram: ProgramText
  ): Unit = {
    withClue(s"when testing markdown: `$markedProgram`") {
      val programAndMarks = TestUtils.ProgramTestCase(markedProgram)
      println(s"testing program: `${programAndMarks.program}`")
      testSpanTreeFor(programAndMarks.program) { root =>
        withClue("insertion points: ") {
          root.insertionPoints shouldEqual programAndMarks.expectedInsertionPoints
        }
        withClue("settableChildren points: ") {
          root.settableChildren.map(_.span) shouldEqual programAndMarks.expectedSettableParts
        }
        withClue("erasable points: ") {
          root.erasableChildren.map(_.span) shouldEqual programAndMarks.expectedErasableParts
        }
      }
    }
  }

  def expectImports(
    value: Seq[Module.Name],
    expected: Module.Name*
  ): Unit = {
    if (value.size != expected.size)
      fail(
        s"Imports list $value has ${value.size} elements"
        + s" while expected ${expected.size}"
      )

    value.zip(expected).foreach {
      case (lhs, rhs) => lhs.nameAsString() shouldEqual rhs.nameAsString()
    }
  }

  /** Traverses SpanTree and checks that text obtained by taking node span's
    * part of node text expressions is the same as pretty-printing of the node.
    *
    * @param tree A root node of the span tree.
    */
  def verifyTreeIndices(tree: SpanTree): Unit = {
    def verifyNode(node: SpanTree): Unit = node match {
      case node: SpanTree.AstNode =>
        val textFromSpan    = tree.text.substring(node.span)
        val textFromShowing = node.ast.show()
        textFromSpan shouldEqual textFromShowing
        node.children.foreach(verifyNode)
      case node: SpanTree.EmptyEndpoint =>
        node.span.length shouldEqual 0
        node.children.foreach(verifyNode)
    }
    verifyNode(tree)
  }
  def verifyTreeIndices(node: Node.Info): Unit = verifyTreeIndices(node.expr)

  def asExpected[Target: ClassTag](value: AnyRef): Target = {
    value shouldBe a[Target]
    value.asInstanceOf[Target]
  }
}

class Tests extends FunSuite with TestUtils {
  test("recognizing lack of imports") {
    withDR("") { dr =>
      val imports = dr.importedModules(mockModule)
      imports should have length 0
    }
  }
  test("recognizing single import") {
    withDR("import Foo.Baz") { dr =>
      val imports = dr.importedModules(mockModule)
      expectImports(imports, Module.Name("Foo.Baz"))
    }
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
      "import Foo.Bar"
    ) {
      _.importModule(mockModule, Module.Name("Foo.Bar"))
    }
  }
  test("removing the only import") {
    checkThatTransforms(
      "import Foo.Baz",
      "",
      _.removeImport(mockModule, Module.Name("Foo.Baz"))
    )
  }
  test("removing one of several imports") {
    checkThatTransforms(
      """import Foo
        |import Foo.Baz
        |import Foo.Bar
        |add x y = x + y""".stripMargin,
      """import Foo
        |import Foo.Bar
        |add x y = x + y""".stripMargin,
      _.removeImport(mockModule, Module.Name("Foo.Baz"))
    )
  }
  test("removing import between empty lines") {
    checkThatTransforms(
      """import Foo
        |
        |import Foo.Baz
        |
        |add x y = x + y""".stripMargin,
      """import Foo
        |
        |
        |add x y = x + y""".stripMargin,
      _.removeImport(mockModule, Module.Name("Foo.Baz"))
    )
  }
  test("get empty module graph") {
    withDR("") { dr =>
      val graph = dr.getGraph(Module.Graph.Location(mockModule))
      graph.nodes should have size 0
      graph.links should have size 0
    }
  }
  test("no nodes from function def") {
    withDR("add a b = a + b") { dr =>
      val graph = dr.getGraph(Module.Graph.Location(mockModule))
      graph.nodes should have size 0
      graph.links should have size 0
    }
  }
//  test("no nodes from operator def") {
//    withDR(
//      "+ a b = a + b",
//      dr => {
//        val graph = dr.getGraph(Module.Graph.Location(mockModule))
//        graph.nodes should have size 0
//        graph.links should have size 0
//      }
//    )
//  }
  test("node trivial literal") {
    checkModuleSingleNodeGraph("15") { node =>
      node.expr.text should equal("15")
      node.inputs shouldEqual None
      node.flags shouldBe empty
    }
  }
  test("node literal in parens") {
    checkModuleSingleNodeGraph("(15)") { node =>
      node.expr.text should equal("(15)")
//      node.inputs should have size 0
      //node.output.name should equal(None)
      node.flags shouldBe empty
    }
  }
  test("node trivial var") {
    checkModuleSingleNodeGraph("foo") { node =>
      node.expr.text should equal("foo")
      node.inputs shouldEqual None
      node.flags shouldBe empty
    }
  }
//  test("node single paren arg app") {
//    checkModuleSingleNodeGraph("foo (4)") { node =>
//      node.expr.text should equal("foo (4)")
//      node.inputs should have size 1
//      node.inputs should equal(Seq(Port.Empty))
//      node.flags shouldBe empty
//    }
//  }
//  test("node two arg app with paren") {
//    checkModuleSingleNodeGraph("(foo a) _") { node =>
//      node.expr.text should equal("(foo a) _")
//      node.inputs should equal(Port.Empty(2))
//      node.flags shouldBe empty
//    }
//  }
  test("get trivial named node") {
    checkModuleSingleNodeGraph("a = 15") { node =>
      node.expr.text should equal("15")
      node.inputs shouldEqual None
      node.flags shouldBe empty
    }
  }
//  // TODO support unary minus
//  test("node unary minus number") {
//    checkJustExpressionSpanTree("-5") { root =>
//      root.visibleChildren.map(_.text) shouldEqual Seq("5")
//    }
//  }
  // TODO: `a+ b` works same as `a + b` but not `a +b`
  //  to be considered how span tree should behave in such conditions
//  test("infix chain with left-attached operator") {
//    checkJustExpressionSpanTree("a+ b + c") { root =>
//      root.visibleChildren.map(_.text) shouldEqual Seq("a", "b", "c")
//    }
  //  }

  test("flattening prefix application") {
    testSpanTreeFor("a b c 4") { root =>
      root.children match {
        case Seq(a_, b, c, d, insertPoint) =>
          a_.text shouldEqual "a"
          b.text shouldEqual "b"
          c.text shouldEqual "c"
          d.text shouldEqual "4"
          insertPoint shouldBe a[SpanTree.EmptyEndpoint]
        case _ =>
          fail(s"wrong element count in: ${root.children}")
      }
      root.children.foreach(_.children should have size 0)
    }
  }
  test("flattening infix chain") {
    testSpanTreeFor("a+b+c+4") { root =>
      root.children match {
        case Seq(a_, opr, b, opr2, c, opr3, d, insertPoint) =>
          a_.text shouldEqual "a"
          opr.text shouldEqual "+"
          b.text shouldEqual "b"
          opr2.text shouldEqual "+"
          c.text shouldEqual "c"
          opr3.text shouldEqual "+"
          d.text shouldEqual "4"
          insertPoint shouldBe a[SpanTree.EmptyEndpoint]
        case _ =>
          fail(s"wrong element count in: ${root.children}")
      }
      root.children.foreach(_.children should have size 0)
    }
  }
  test("flattening right-associative infix chain") {
    testSpanTreeFor("a , b , c , 4") { root =>
      root.children match {
        case Seq(a_, opr, b, opr2, c, opr3, d, insertPoint) =>
          a_.text shouldEqual "a"
          opr.text shouldEqual ","
          b.text shouldEqual "b"
          opr2.text shouldEqual ","
          c.text shouldEqual "c"
          opr3.text shouldEqual ","
          d.text shouldEqual "4"
          insertPoint shouldBe a[SpanTree.EmptyEndpoint]
        case _ =>
          fail(s"wrong element count in: ${root.children}")
      }
      root.children.foreach(_.children should have size 0)
    }
  }
  test("flattening infix application2") {
    testSpanTreeFor("a +  b *  c + d") { root =>
      root.children match {
        case Seq(a_, opr, bTimesC, opr2, d, insertPoint) =>
          a_.text shouldEqual "a"
          opr.text shouldEqual "+"
          bTimesC.text shouldEqual "b *  c"
          opr2.text shouldEqual "+"
          d.text shouldEqual "d"
          insertPoint shouldBe a[SpanTree.EmptyEndpoint]

          bTimesC shouldBe a[SpanTree.OperatorChain]
          bTimesC
            .asInstanceOf[SpanTree.OperatorChain]
            .opr
            .name shouldEqual "*"
          bTimesC.children match {
            case Seq(b, times, c, nestedInsertPoint) =>
              b shouldBe a[SpanTree.AstLeaf]
              b.text shouldEqual "b"
              times shouldBe a[SpanTree.AstLeaf]
              times.text shouldEqual "*"
              b shouldBe a[SpanTree.AstLeaf]
              c.text shouldEqual "c"
              insertPoint shouldBe a[SpanTree.EmptyEndpoint]
            case _ => fail(s"wrong children count for ${bTimesC.text}")
          }

        case _ =>
          fail(s"wrong element count in: ${root.children}")
      }
    }
  }

  /////////////////////////////////
  def checkNodeHasNoOutputs(program: ProgramText): Unit =
    checkNodeOutputs(program)(_ shouldEqual OutputTree.Anonymous)

  def checkNodeOutputs[A](program: ProgramText)(f: OutputTree => A): A = {
    checkModuleSingleNodeGraph(program) { node =>
      node.outputs match {
        case Some(output) => f(output)
        case None =>
          fail(s"missing output information for `$program`")
      }
    }
  }
  test("no node outputs when no assignment") {
    val programs =
      Seq("15", "foo", "_", "20+79 * aa", "foo.bar.baz", "import Base")
    programs foreach checkNodeHasNoOutputs
  }
  test("node outputs on single var assignment") {
    type ExpectedVarName = String
    val cases: Seq[(ProgramText, ExpectedVarName)] =
      Seq("a = 15" -> "a", "bar = baz" -> "bar")
    cases.foreach {
      case (program, expectedVar) =>
        checkNodeOutputs(program)(_ shouldEqual OutputTree.Var(expectedVar))
    }
  }
  test("nodes with multiple outputs") {
    type ExpectedVarName = String
    val cases: Seq[(ProgramText, Seq[ExpectedVarName])] = Seq(
      "a,b = foo"   -> Seq("a", "b"),
      "a,b,c = foo" -> Seq("a", "b", "c")
    )
    cases.foreach {
      case (program, expectedVars) =>
        val expectedSubPorts = expectedVars.map(OutputTree.Var(_))
        val expectedPort     = OutputTree.PortGroup(expectedSubPorts)
        checkNodeOutputs(program)(_ shouldEqual expectedPort)
    }

    // , a b = ...
  }

  /////////////////////////////////////////////////////////////////////

  test("prefix application actions") {
    testSpanTreeMarkdown("foo")
    testSpanTreeMarkdown("foo ⎀«a»⎀")
    testSpanTreeMarkdown("foo ⎀«4»⎀")
    testSpanTreeMarkdown("foo ⎀«a» ⎀«_»⎀")
    // can insert before each argument, after `b`, after `foo a b`
  }

  test("infix operator chains") {
    testSpanTreeMarkdown("⎀+⎀")
    testSpanTreeMarkdown("⎀«a»+⎀")
    testSpanTreeMarkdown("⎀+⎀«b»⎀")
    testSpanTreeMarkdown("⎀+⎀«5»⎀")
    testSpanTreeMarkdown("⎀+⎀«b»+⎀")
    testSpanTreeMarkdown("⎀+⎀«b»+⎀«c»⎀")
    testSpanTreeMarkdown("⎀«a»+⎀«b»+⎀")
    testSpanTreeMarkdown("⎀«a»+⎀«b»⎀")
    testSpanTreeMarkdown("⎀+⎀ +⎀")
    testSpanTreeMarkdown("⎀+⎀«a»+⎀«b»+⎀")
  }

  /////////////////////////////////////////////////////////////////////

//  def checkNodeInputs[A](program: ProgramText)(f: InputTree => A): A = {
//    checkModuleSingleNodeGraph(program) { node =>
//      node.inputs match {
//        case Some(output) => f(output)
//        case None =>
//          fail(s"missing intput information for `$program`")
//      }
//    }
//  }
//
//  test("inputs: foo bar baz") {
//    fail("FIXME")
////    checkNodeInputs("foo bar baz") {
////      case InputGroup(Seq(), _, Seq(a, b, c, d, e)) =>
////        println("ha!")
////      case otherInputs =>
////        fail(s"wrong inputs: $otherInputs")
////    }
//  }

//  test("foo") {
//    val ast = ParserUtils.parse("+ a b = foo")
////    val ast  = ParserUtils.parse("1,2")
//    val ast2 = ParserUtils.parse("1, 2")
//    val ast3 = ParserUtils.parse("1 ,2")
//    val ast4 = ParserUtils.parse("1 , 2")
//    println(ast.toString)
//  }
}
