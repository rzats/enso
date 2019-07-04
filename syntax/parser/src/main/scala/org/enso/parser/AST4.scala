package org.enso.syntax4

object AST {

  trait Convertible[-Source, +Target] {
    def convert(source: Source): Target
  }

  final class CodeBuilder {
    val stringBuilder: StringBuilder = new StringBuilder()
    var indent: Int                  = 0

    def +=(char: Char): Unit =
      stringBuilder += char

    def ++=(str: String): Unit =
      stringBuilder ++= str

    def newline(): Unit = {
      stringBuilder += '\n'
      stringBuilder ++= " " * indent
    }

    def result(): String =
      stringBuilder.result()

    def withIndent[T](newIndent: Int)(f: => T): T = {
      val oldIndent = indent
      indent = newIndent
      val out = f
      indent = oldIndent
      out
    }

    def withIndentDiff[T](indentDiff: Int): (=> T) => T =
      withIndent(indent + indentDiff)

  }

  ////////////////////
  ////// Symbol //////
  ////////////////////

  trait Symbol {
    def span: Int

    def show(out: CodeBuilder): Unit = {
      out ++= show()
    }

    def show(): String = {
      val out = new CodeBuilder
      show(out)
      out.result()
    }

    def to[A](implicit converter: Convertible[this.type, A]): A =
      converter.convert(this)
  }

  case class Fix[F[_] <: Symbol](unfix: F[Fix[F]]) extends Symbol {
    override def span:                   Int  = unfix.span
    override def show(out: CodeBuilder): Unit = unfix.show(out)
  }

  /////////////////
  ////// AST //////
  /////////////////

  trait AST[T]        extends Symbol
  trait InvalidAST[T] extends AST[T]

  type FAST = Fix[AST]

  final case class Invalid[T](symbol: InvalidAST[T]) extends AST[T] {
    override def span:                   Int  = symbol.span
    override def show(out: CodeBuilder): Unit = symbol.show(out)
  }

  implicit final class _OptionAST_[T](val self: Option[AST[T]]) extends Symbol {
    override def span:                   Int  = self.map(_.span).getOrElse(0)
    override def show(out: CodeBuilder): Unit = self.foreach(_.show(out))
  }

  /////// Unrecognized //////

  final case class Unrecognized[T](str: String) extends InvalidAST[T] {
    override def span:                   Int  = str.length
    override def show(out: CodeBuilder): Unit = out ++= str
  }

  ////// Identifiers //////

  trait Identifier[T] extends AST[T]

  final case class Wildcard[T]() extends Identifier[T] {
    override def span:                   Int  = 1
    override def show(out: CodeBuilder): Unit = out += '_'
  }

  final case class Var[T](name: String) extends Identifier[T] {
    override def span:                   Int  = name.length
    override def show(out: CodeBuilder): Unit = out ++= name
  }
  object Var {
    def apply(name: String): FAST = Fix(Var(name))
  }

  final case class Cons[T](name: String) extends Identifier[T] {
    override def span:                   Int  = name.length
    override def show(out: CodeBuilder): Unit = out ++= name
  }

  final case class Operator[T](name: String) extends Identifier[T] {
    override def span:                   Int  = name.length
    override def show(out: CodeBuilder): Unit = out ++= name
  }

  final case class Modifier[T](name: String) extends Identifier[T] {
    override def span: Int = name.length + 1
    override def show(out: CodeBuilder): Unit = {
      out ++= name
      out += '='
    }
  }

  final case class InvalidSuffix[T](elem: Identifier[T], suffix: String)
      extends InvalidAST[T] {
    override def span: Int = elem.span + suffix.length
    override def show(out: CodeBuilder): Unit = {
      elem.show(out)
      out ++= suffix
    }
  }

  ////// Operations //////

  final case class App[T <: Symbol](func: T, offset: Int, arg: T)
      extends AST[T] {
    override def span: Int = func.span + offset + arg.span
    override def show(out: CodeBuilder): Unit = {
      func.show(out)
      out ++= " " * (span - func.span - arg.span)
      arg.show(out)
    }
  }
  object App {
    def apply(func: FAST, offset: Int, arg: FAST): FAST =
      Fix(new App(func, offset, arg))
  }

  final case class Group[T](leftOffset: Int, body: AST[T], rightOffset: Int)
      extends AST[T] {
    override def span: Int = leftOffset + body.span + rightOffset + 2
    override def show(out: CodeBuilder): Unit = {
      out += '('
      out ++= " " * leftOffset
      body.show(out)
      out ++= " " * rightOffset
      out += ')'
    }
  }

  //  val test: Fix[AST] = Fix(App(Var("x"), 2, Var("y")))

  //  val test: FAST = App(Var("x"), 1, Var("y"))
  ////// Number //////

  final case class Number[T](
    base: Option[String],
    int: String
    //    frac: Option[String]
  ) extends AST[T] {
    override def span: Int = {
      val baseSpan = base.map(_.length + 1).getOrElse(0)
      val intSpan  = int.length
      //      val fracSpan = frac.map(_.length + 1).getOrElse(0)
      baseSpan + intSpan // + fracSpan
    }
    override def show(out: CodeBuilder): Unit = {
      val pfx = base.map(_ + "_").getOrElse("")
      //      val sfx = frac.map("." + _).getOrElse("")
      out ++= pfx + int // + sfx
    }
  }

  object Number {
    def int[T](int: String): Number[T] = Number(None, int) //, None)

    def basedInt[T](base: String, int: String): Number[T] =
      Number(Some(base), int) //, None)

    final case class DanglingBase[T](base: String) extends InvalidAST[T] {
      override def span: Int = base.length + 1
      override def show(out: CodeBuilder): Unit = {
        out ++= base
        out += '_'
      }
    }
  }

  ////// Text //////

  final case class Text[T](segments: Vector[TextSegment]) extends AST[T] {

    override def span: Int =
      2 + segments.foldLeft(0)((i, s) => i + s.span)

    override def show(out: CodeBuilder): Unit = {
      out += '\''
      segments.foreach(_.show(out))
      out += '\''
    }
  }

  trait TextSegment extends Symbol

  final case class PlainTextSegment(value: String) extends TextSegment {
    override def span: Int =
      value.length

    override def show(out: CodeBuilder): Unit =
      out ++= value
  }

  ////// Block //////

  final case class Block[T](
    indent: Int,
    emptyLines: Vector[Int],
    firstLine: RequiredLine[T],
    lines: Vector[Line[T]]
  ) extends AST[T] {

    def linesCount: Int =
      emptyLines.length + lines.length + 1

    def linesSpan: Int =
      emptyLines.sum + lines.foldLeft(firstLine.span)((i, a) => i + a.span)

    override def span: Int = {
      val newlinesSpan = linesCount
      val indentSpan   = linesCount * indent
      linesSpan + newlinesSpan + indentSpan
    }

    override def show(out: CodeBuilder): Unit = {
      val globalIndent    = indent + out.indent
      val globalIndentStr = " " * globalIndent
      out.withIndent(globalIndent) {
        out += '\n'
        emptyLines.foreach(i => out ++= globalIndentStr + " " * i + "\n")
        out ++= globalIndentStr
        firstLine.show(out)
        lines.foreach { line =>
          out += '\n'
          out ++= globalIndentStr
          line.show(out)
        }
      }
    }
  }

  final case class RequiredLine[T](span: Int, elem: AST[T]) extends Symbol {}

  final case class Line[T](span: Int, elem: Option[AST[T]]) extends Symbol {
    override def show(out: CodeBuilder): Unit = {
      elem.show(out)
      out ++= " " * (span - elem.span)
    }
  }

  object Line {
    def empty(span: Int) = Line(span, None)
  }

  implicit object RequiredLine_to_Line
      extends Convertible[RequiredLine[_], Line[_]] {
    def convert(src: RequiredLine[_]): Line[_] = Line(src.span, Some(src.elem))
  }

  final case class InvalidBlock[T](block: Block[T]) extends InvalidAST[T] {
    override def span:                   Int  = block.span
    override def show(out: CodeBuilder): Unit = block.show(out)
  }

  ////// Unit //////

  final case class Module[T](firstLine: Line[T], lines: List[Line[T]])
      extends AST[T] {
    override def span: Int =
      lines.foldLeft(firstLine.span) { case (s, l) => s + l.span }
    override def show(out: CodeBuilder): Unit = {
      firstLine.show(out)
      lines.foreach { line =>
        out += '\n'
        line.show(out)
      }
    }
  }

  object Module {
    //    def oneLiner(line: Line): Module = Module(line, List())
  }

  ////////////////////////////////
  ////// Smart Constructors //////
  ////////////////////////////////

  //  def app[T](fn: AST[T], arg: AST[T]): AST[T] = app(fn, 1, arg)
  //  def app[T](fn: AST[T], offset: Int, arg: AST[T]): AST[T] =
  //    App(fn, offset, arg)

  def block[T](
    indent: Int,
    emptyLines: List[Int],
    firstLine: RequiredLine[T],
    lines: List[Line[T]]
    //    valid: Boolean
  ): Block[T] = {
    val vEmptyLines = emptyLines.to[Vector]
    val vLines      = lines.to[Vector]
    Block(indent, vEmptyLines, firstLine, vLines)
    //    val block       = Block(indent, vEmptyLines, firstLine, vLines)
    //    if (valid) block else Invalid(InvalidBlock(block))
  }

}

//import shapeless.Lazy
//
//sealed trait AST[T]
//case class Var[T](s: String) extends AST[T]
//case class App[T](a: T, i: Int, b: T) extends AST[T]
//
//case class Fix[F[_]](unfix: F[Fix[F]])
//
//trait HasSpan[T] {
//  def span(t: T): Int
//}
//
//
//object HasSpan {
//
//  implicit def AST_Span[T](implicit ev: Lazy[HasSpan[T]]): HasSpan[AST[T]] =
//    (t: AST[T]) => t match {
//      case Var(s) => s.length
//      case App(a, i, b) => ev.value.span(a) + i + ev.value.span(b)
//    }
//
//  implicit def F_Span[F[_]](implicit ev: Lazy[HasSpan[F[Fix[F]]]]): HasSpan[Fix[F]] =
//    (f: Fix[F]) => ev.value.span(f.unfix)
//
//}
//def test: Fix[AST] = Fix(Var("x"))
//
//def a = implicitly[HasSpan[Fix[AST]]].span(test)
//
//println(a)
