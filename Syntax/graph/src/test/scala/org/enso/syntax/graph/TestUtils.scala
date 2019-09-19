package org.enso.syntax.graph

import org.enso.syntax.graph.API._
import org.enso.syntax.graph.Ops._
import org.enso.syntax.graph.AstOps._

import scala.reflect.ClassTag

final case class NotificationSinkMock() extends NotificationSink {
  var notificationsReceived: Seq[API.Notification] = Seq()
  override def notify(notification: API.Notification): Unit = {
    println(s"Got notification: $notification")
    notificationsReceived = notificationsReceived :+ notification
  }
}

trait TestUtils extends org.scalatest.Matchers {
  type ProgramText = String

  val mockModule = StateManagerMock.mainModule
  def withDR[R](program: ProgramText)(
    action: DoubleRepresentation => R
  ): (R, StateManagerMock, NotificationSinkMock) = {
    val state    = StateManagerMock(program)
    val notifier = NotificationSinkMock()
    val result   = action(DoubleRepresentation(state, notifier))
    (result, state, notifier)
  }

  def checkOutput[R](
    program: String,
    expectedOutput: R,
    action: DoubleRepresentation => R
  ): R = {
    val (result, _, _) = withDR(program)(action)
    result should be(expectedOutput)
    result
  }

  def checkThatTransforms[R](
    initialProgram: String,
    expectedProgram: String,
    action: DoubleRepresentation => R,
    notification: Option[API.Notification] = None
  ): R = {
    val (result, state, notifier) = withDR(initialProgram)(action)
    state.ast.show should be(ParserUtils.preprocess(expectedProgram))
    notification.foreach(notifier.notificationsReceived.head should be(_))
    result
  }

  def checkTextModifications(program: String): Unit = {
    val prefix = program.takeWhile(_ != '»').dropRight(1)
    val suffix = program.dropWhile(_ != '«').drop(2)
    val middle = program.dropWhile(_ != '»').drop(1).takeWhile(_ != '«')

    val position = TextPosition(prefix.length)
    val span     = TextSpan(position, TextLength(middle.length))

    checkThatTransforms(
      prefix + suffix,
      prefix + middle + suffix,
      _.insertText(mockModule, position, middle),
      Some(TextAPI.Notification.Inserted(mockModule, position, middle))
    )
    checkThatTransforms(
      prefix + middle + suffix,
      prefix + suffix,
      _.eraseText(mockModule, span),
      Some(TextAPI.Notification.Erased(mockModule, span))
    )
    checkOutput(
      prefix + middle + suffix,
      middle,
      _.copyText(mockModule, span)
    )
  }

  def expectTransformationError[E: ClassTag](
    initialProgram: String
  )(action: DoubleRepresentation => Unit): Unit = {
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
  def testSpanTreeFor[R](program: ProgramText)(action: SpanTree => R): R =
    checkModuleSingleNodeGraph(program) { node =>
      // make sure that span covers the whole expression
      val expectedSpan =
        TextSpan(TextPosition.Start, TextLength(program))
      node.expr.span shouldEqual expectedSpan

      verifyTreeIndices(node.expr)
      action(node.expr)
    }

  def testSpanTreeMarkdown[R](markedProgram: ProgramText): Unit = {
    withClue(s"when testing markdown: `$markedProgram`") {
      val programAndMarks = SpanTreeTestCase(markedProgram)
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
    def verifyNode(node: SpanTree): Unit = {
//      println(s"Checking node $node")
      val textFromSpan = tree.text.substring(node.span)
      textFromSpan shouldEqual node.text
      node match {
        case node: SpanTree.AstNode =>
          textFromSpan shouldEqual node.ast.show()
        case node: SpanTree.EmptyEndpoint =>
          node.span.length shouldEqual TextLength.Empty
        case _ =>
          Unit
      }
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

case class SpanTreeTestCase(
  program: String,
  expectedInsertionPoints: Seq[TextPosition],
  expectedSettableParts: Seq[TextSpan],
  expectedErasableParts: Seq[TextSpan]
)

object SpanTreeTestCase {

  val InsertionMark                 = '⎀'
  val BeginSettableAndErasableBlock = '«'
  val EndSettableAndErasableBlock   = '»'
  val BeginSettableBlock            = '‹'
  val EndSettableBlock              = '›'
  val markdownChars =
    Seq(
      InsertionMark,
      BeginSettableAndErasableBlock,
      EndSettableAndErasableBlock,
      BeginSettableBlock,
      EndSettableBlock
    )

  def apply(markedProgram: String): SpanTreeTestCase = {
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

    SpanTreeTestCase(program, insertionPoints, settableSpans, erasableSpans)
  }
}
