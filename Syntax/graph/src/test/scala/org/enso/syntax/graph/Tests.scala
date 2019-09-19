package org.enso.syntax.graph

import org.enso.syntax.graph.API._
import org.scalatest._

class Tests extends FunSuite with TestUtils {
  test("text modifications") {
    List(
      "a (»«) c",
      "a (»1 2«) c",
      "'text(»'«)",
      "if a then b(» then x«) else c",
      "((( a (»+ b«) )))"
    ).foreach(checkTextModifications)
  }

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
  test("no nodes from import") {
    withDR("import Foo.Bar") { dr =>
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
  test("atom in in brackets") {
    case class AtomInBrackets(literal: String, loff: Int, roff: Int) {
      val leftSegment  = "(" + " " * loff + literal
      val rightSegment = ")"
      val program      = leftSegment + " " * roff + ")"
    }

    val cases = Seq(
      AtomInBrackets("15", 0, 0),
      AtomInBrackets("foo", 1, 1),
      AtomInBrackets("15", 2, 0),
      AtomInBrackets("_", 0, 0),
      AtomInBrackets("15", 0, 2)
    )

    cases.foreach { testedCase =>
      println("testing span tree structure for " + testedCase.program)
      testSpanTreeFor(testedCase.program) { tree =>
        val children = asExpected[SpanTree.MacroMatch](tree).describeChildren
        children should have size 2
        children match {
          case Seq(leftInfo, rightInfo) =>
            val rightSegment =
              asExpected[SpanTree.MacroSegment](rightInfo.spanTree)
            val leftSegment =
              asExpected[SpanTree.MacroSegment](leftInfo.spanTree)

            leftSegment.text shouldEqual testedCase.leftSegment
            leftInfo.actions shouldEqual Set()

            val leftChildren = leftSegment.describeChildren
            leftChildren should have size 1
            val bracketContents = leftChildren.head
            val leftChildNode =
              asExpected[SpanTree.AstLeaf](bracketContents.spanTree)
            leftChildNode.text shouldEqual testedCase.literal
            //bracketContents.actions shouldEqual Set(SpanTree.Action.Insert)

            rightSegment.text shouldEqual testedCase.rightSegment
            rightInfo.actions shouldEqual Set()
        }
      }
    }
  }
  test("node trivial var") {
    checkModuleSingleNodeGraph("foo") { node =>
      node.expr.text should equal("foo")
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
            case _ =>
              fail(s"wrong children count for ${bTimesC.text}")
          }

        case _ =>
          fail(s"wrong element count in: ${root.children}")
      }
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
  // test("infix chain with left-attached operator") {
  //    checkJustExpressionSpanTree("a+ b + c") { root =>
  //      root.visibleChildren.map(_.text) shouldEqual Seq("a", "b", "c")
  //    }
  //  }

  ////////////////////
  // OUTPUTS /////////
  ////////////////////s
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
      Seq("15", "foo", "_", "20+79 * aa", "foo.bar.baz")
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
}
