package org.enso.syntax.text.precedence

import org.enso.data.List1
import org.enso.data.Shifted
import org.enso.data
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST._

import scala.annotation.tailrec
import cats.implicits._
import org.enso.syntax.text.AST.Template.Segment.Pattern
import org.enso.syntax.text.ast.Repr

object Template {

  val Template = AST.Template
  import Template._

  def exprList(ast: AST): Shifted.List1[AST] = {
    @tailrec
    def go(ast: AST, out: AST.Stream): Shifted.List1[AST] = ast match {
      case _App(fn, off, arg) => go(fn, Shifted(off, arg) :: out)
      case ast                => Shifted.List1(ast, out)
    }
    go(ast, List())
  }

  //////////////////
  //// Registry ////
  //////////////////

  final case class Registry() {
    var tree: Registry.Tree = data.Tree()

    override def toString: String =
      tree.toString

    def insert(t: Definition.Spec[Definition]): Unit =
      tree += t.el.segments.toList.map(_._1) -> t.map(_.segments.map(_._2))

    def get(path: List1[AST]): Option[Registry.Value] =
      tree.getValue(path.toList)

  }

  object Registry {
    type Value = Definition.Spec[List1[Template.Segment.Pattern]]
    type Tree  = data.Tree[AST, Value]
    def apply(ts: Definition.Spec[Definition]*): Registry = {
      val registry = new Registry()
      ts.foreach(registry.insert)
      registry
    }
  }

  /////////////////
  //// Context ////
  /////////////////

  case class Context(tree: Registry.Tree, parent: Option[Context]) {
    def get(t: AST): Option[Registry.Tree] =
      tree.get(t)

    def isEmpty: Boolean =
      tree.isEmpty

    @tailrec
    final def parentCheck(t: AST): Boolean = {
      parent match {
        case None => false
        case Some(p) =>
          p.get(t) match {
            case None    => p.parentCheck(t)
            case Some(_) => true
          }
      }
    }
  }

  object Context {
    def apply():                    Context = Context(data.Tree(), None)
    def apply(tree: Registry.Tree): Context = Context(tree, None)
  }

  /////////////////
  //// Builder ////
  /////////////////

  class SegmentBuilder(val ast: Ident) {
    import Template.Segment.Pattern
    import Template._

    var offset: Int         = 0
    var revBody: AST.Stream = List()

    def buildAST() = revBody match {
      case Nil           => None
      case seg1 :: seg2_ => Some(Operator.rebuild(List1(seg1, seg2_)))
    }

    def buildAST2(revLst: AST.Stream): Option[Shifted[AST]] =
      revLst match {
        case Nil           => None
        case seg1 :: seg2_ => Some(Operator.rebuild(List1(seg1, seg2_)))
      }

    def build(tp: Pattern): Shifted[Segment] = {
      val stream = revBody.reverse
      val segment2 = resolveStep(tp, stream) match {
        case None => Segment.Unmatched(tp, ast, stream)
        case Some(rr) =>
          rr.stream match {
            case Nil     => Segment.Valid(ast, rr.elem)
            case s :: ss => Segment.Unsaturated(ast, rr.elem, List1(s, ss))
          }
      }

      Shifted(offset, segment2)
    }

    //////////////////////////////////////

    case class ResolveResult(elem: Pattern.Match_, stream: AST.Stream) {
      def map(fn: Pattern.Match_ => Pattern.Match_): ResolveResult =
        copy(elem = fn(elem))
    }

    def resolveList(
      p: Pattern,
      stream: AST.Stream
    ): (List[Pattern.Match_], AST.Stream) = {
      @tailrec
      def go(
        stream: AST.Stream,
        revOut: List[Pattern.Match_]
      ): (List[Pattern.Match_], AST.Stream) =
        resolveStep(p, stream) match {
          case None    => (revOut.reverse, stream)
          case Some(t) => go(t.stream, t.elem :: revOut)
        }
      go(stream, Nil)
    }

    def resolveStep(p: Pattern, stream: AST.Stream): Option[ResolveResult] = {
      import Pattern._

      def ret2[S: Repr.Of](
        pat: Pattern.Of[S],
        res: S,
        stream: AST.Stream
      ) =
        Some(ResolveResult(Pattern.Match(pat, res), stream))

      p match {
        case p @ Pattern.Nothing() =>
          ret2(p, (), stream)

        case p @ Pattern.Tag(tag, pat2) =>
          resolveStep(pat2, stream).map(_.map(Pattern.Match(p, _)))

        case p @ Pattern.Build(pat2) =>
          resolveStep(pat2, stream).map(
            _.map(x => Pattern.Match(p, buildAST2(x.toStream.reverse).get))
          )

        case p @ Pattern.Seq(pat1, pat2) =>
          resolveStep(pat1, stream) match {
            case None => None
            case Some(r1) =>
              resolveStep(pat2, r1.stream) match {
                case None => None
                case Some(r2) =>
                  ret2(p, (r1.elem, r2.elem), r2.stream)
              }
          }

        case p @ Pattern.Cls() =>
          stream match {
            case Shifted(off, p.tag(t)) :: ss =>
              ret2(p, Shifted(off, t), ss)
            case _ => None
          }

        case p @ Pattern.Many(p2) =>
          val (lst, stream2) = resolveList(p2, stream)
          ret2(p, lst, stream2)

        case p @ Pattern.Opt(p2) =>
          resolveStep(p2, stream) match {
            case None    => ret2(p, None, stream)
            case Some(r) => ret2(p, Some(r.elem), r.stream)
          }

        case p @ Or(p1, p2) =>
          resolveStep(p1, stream) match {
            case Some(t) => Some(t)
            case None    => resolveStep(p2, stream)
          }

        case p @ Pattern.Tok(tok) =>
          stream match {
            case Shifted(off, t) :: ss =>
              if (tok == t) ret2(p, Shifted(off, t), ss) else None
            case _ => None
          }

        case p @ Pattern.Err(msg, p1) =>
          resolveStep(p1, stream).map(
            _.map(
              x => Pattern.Match(p, Shifted(AST.Unexpected(msg, x.toStream)))
            )
          )

        case p @ Not(p1) =>
          resolveStep(p1, stream) match {
            case Some(_) => None
            case None    => ret2(p, (), stream)
          }
      }
    }

    override def toString: String =
      s"SegmentBuilder($offset, $revBody)"
  }

  class MixfixBuilder(ast: Ident) {
    var context: Context               = Context()
    var mixfix: Option[Registry.Value] = None
    var current: SegmentBuilder        = new SegmentBuilder(ast)
    var revSegs: List[SegmentBuilder]  = List()
  }

  def mkBuiltInRegistry(): Registry = {

    def internalError = throw new Error("Internal error")

    import Template.Segment.Pattern

    val groupDef = Definition.Restricted(
      Opr("(") -> Pattern.Opt(Pattern.Cls[Ident]),
      Opr(")") -> Pattern.Nothing()
    ) {
      case List(s1, s2) =>
        s1.el.body.toStream match {
          case List()  => AST.Group()
          case List(t) => AST.Group(t)
          case _       => internalError
        }
      case _ => internalError
    }
//
//    val defDef = Definition.Unrestricted(
//      Var("def") -> Pattern.Seq(
//        Pattern.Opt(Pattern.TokenCls[AST.Cons]),
//        Pattern.Opt(Pattern.Many(Pattern.NotTokenCls[AST.Block])),
//        Pattern.Opt(Pattern.TokenCls[AST.Block])
//      )
//    ) {
//      case List(s1) =>
//        s1.el.body match {
//          case Many(lst) =>
//            val nameParam = lst(0)
//            val argsParam = lst(1)
//            val bodyParam = lst(2)
//            val name = nameParam match {
//              case Expr(Shifted(off, t @ AST.Cons(_))) =>
//                Shifted(off, t)
//              case Empty => Shifted(AST.Missing)
//              case _     => throw new Error("Internal Parser Error")
//            }
//            val args = argsParam.toStream()
//            val body: Option[Shifted[AST]] = bodyParam match {
//              case Expr(n @ Shifted(_, _: AST.Block)) => Some(n)
//              case Empty                              => None
//              case _ =>
//                throw new Error("Internal Parser Error")
//            }
//            AST.Def(name, args, body)
//        }
//    }

//    def seqSplit[L, R <: L](lst: List[Either[L, R]]): Either[List[L], List[R]] =
//      lst.sequence match {
//        case Right(t) => Right(t)
//        case Left(_)  => Left(lst.map(_.merge))
//      }
//
//    def splitOn[T, S <: T](lst: List[T])(
//      test: T => Option[S]
//    ): (List[List[T]], List[S]) = {
//      @tailrec
//      def go(
//        lst: List[T],
//        current: List[T],
//        chunks: List[List[T]],
//        divs: List[S]
//      ): (List[List[T]], List[S]) =
//        lst match {
//          case Nil => (current :: chunks, divs)
//          case l :: ls =>
//            test(l) match {
//              case Some(l) => go(ls, List(), current :: chunks, l :: divs)
//              case None    => go(ls, l :: current, chunks, divs)
//            }
//        }
//      go(lst, Nil, Nil, Nil)
//    }

    val importDef = Definition.Unrestricted(
      Var("import") ->
      Pattern.SepList(Pattern.Cls[Cons], AST.Opr("."), "expected module name")
    ) {
      case List(s1) =>
        import Pattern.Match._
        val body = s1.el.body
        if (!body.isValid) {
          val toks = Shifted(s1.el.head) :: body.toStream
          AST.Unexpected("wrong import statement", toks)
        } else {
          s1.el.body match {
            case Seq(headMatch, Many(tailMatch)) =>
              def unwrapSeg(lseg: Pattern.Match_): Cons =
                lseg.toStream match {
                  case List(Shifted(_, t @ Cons(_))) => t
                  case _                             => internalError
                }
              val head = unwrapSeg(headMatch)
              val tail = tailMatch.map {
                case Seq(Tok(Shifted(_, Opr("."))), seg) => unwrapSeg(seg)
                case _                                   => internalError
              }
              AST.Import(head, tail)
          }
        }
      case _ => internalError
    }

    val ifThenDef = Definition.Unrestricted(
      Var("if")   -> Pattern.Expr(),
      Var("then") -> Pattern.Expr()
    ) {
      case List(s1, s2) =>
        val body1   = s1.el.body
        val body2   = s2.el.body
        val isValid = body1.isValid && body2.isValid
        if (!isValid) {
          val stream = s1.el.toStream ++ s2.el.toStream
          AST.Unexpected("Invalid conditional statement", stream)
        } else {
          (body1.toStream, body2.toStream) match {
            case (List(t1), List(t2)) =>
              AST.Mixfix(
                List1(s1.el.head, List(s2.el.head)),
                List1(t1.el, List(t2.el))
              )
            case _ => internalError
          }
        }
      case _ => internalError
    }

    Registry(
      groupDef,
      ifThenDef,
//      Definition.Unrestricted(
//        Var("if")   -> Pattern.Expr,
//        Var("then") -> Pattern.Expr,
//        Var("else") -> Pattern.Expr
//      )(a => ???),
      importDef
//      defDef
    )
  }

  val hardcodedRegistry = mkBuiltInRegistry()

  def partition(t: AST): AST = {

    var builder: MixfixBuilder = new MixfixBuilder(Blank)
    builder.mixfix = Some(
      Definition.Spec(
        Definition.Scope.Unrestricted, {
          case Shifted(_, Segment.Valid(_, Pattern.Match(Pattern.Expr(), e))) :: Nil =>
            ??? //(e: AST)

          case _ => throw new scala.Error("Impossible happened")
        },
        List1(Template.Segment.Pattern.Expr(), Nil)
      )
    )
    var builderStack: List[MixfixBuilder] = Nil

    def pushBuilder(ast: Ident, off: Int): Unit = {
//      println(s"pushBuilder($off)")
      builderStack +:= builder
      builder                = new MixfixBuilder(ast)
      builder.current.offset = off
    }

    def popBuilder(): Unit = {
//      println("popBuilder")
      builder      = builderStack.head
      builderStack = builderStack.tail
    }

    def pushSegment(ast: Ident, off: Int): Unit = {
//      println(s"pushSegment($off)")
      builder.revSegs ::= builder.current
      builder.current        = new SegmentBuilder(ast)
      builder.current.offset = off
    }

    import Template._

    val root = Context(hardcodedRegistry.tree)

    def stripLastSegment(
      scope: Definition.Scope,
      revSegs: List1[Shifted[Template.Segment]]
    ): (List1[Shifted[Template.Segment]], AST.Stream) = {
      if (scope == Definition.Scope.Unrestricted)
        (revSegs.reverse, List())
      else {
        val lastSeg                = revSegs.head
        val (lastSegEl, revStream) = lastSeg.el.strip()
        val lastSeg2               = Shifted(lastSeg.off, lastSegEl)
        val revSegments            = List1(lastSeg2, revSegs.tail)
        (revSegments.reverse, revStream.reverse)
      }
    }

    def close(): AST.Stream1 = {
//      println(s"\n\n-----------------------------------\n\n")
      val revSegBldrs = List1(builder.current, builder.revSegs)
//      println(s"revSegs =")
//      pprint.pprintln(revSegs, width    = 50, height = 10000)
//      pprint.pprintln(builder.mixfix, width = 50, height = 10000)
      val result = {
        builder.mixfix match {
          case None =>
            val revSegs = revSegBldrs.map { segBldr =>
              val optAst = segBldr.buildAST()
              Shifted(segBldr.offset, Partial.Segment(segBldr.ast, optAst))
            }
            val segments = revSegs.reverse
            val head     = segments.head
            val tail     = segments.tail
            val paths    = builder.context.tree.dropValues()
            val template = Partial(Shifted.List1(head.el, tail), paths)
            val newTok   = Shifted(head.off, template)
            List1(newTok)

          case Some(ts) =>
            val revSegTps      = ts.el.reverse
            val revSegs        = revSegBldrs.zipWith(revSegTps)(_.build(_))
            val (segs, stream) = stripLastSegment(ts.scope, revSegs)
            val shiftSegs      = Shifted.List1(segs.head.el, segs.tail)
            val optValSegs     = Template.validate(shiftSegs)

            val template = optValSegs match {
              case None => Template.Invalid(shiftSegs)
              case Some(validSegs) =>
                val validSegsList = validSegs.toList()
//                val ast           = ts.finalizer(validSegsList)
                Template.Valid(validSegs) //, ast)
            }

            val newTok = Shifted(segs.head.off, template)

            stream match {
              case Nil     => List1(newTok)
              case s :: ss => List1(s, ss) :+ newTok
            }
        }
      }

//      println("Close Result:")
//      pprint.pprintln(result, width = 50, height = 10000)
      result
    }

    def close2() = {
//      println("close2")
      val subAst = close()
      popBuilder()
      builder.current.revBody = subAst.concat(builder.current.revBody).toList
    }

//    def extractRoot[T](seg: Template.Segment[T]): Unit = {
//      import Template.Segment._
//      import Template.Segment
//      seg match {
//        case Segment(Pattern.Option(Pattern.Expr), _, body) => body.get
//      }
//    }

    @tailrec
    def go(input: AST.Stream): AST = {
//      import Template.Segment.Body._
      input match {
        case Nil =>
          if (builderStack.isEmpty) {
//            println("End of input (not in stack)")
            close().head.el match {
              case Template.Valid(segs) =>
                segs.head.body.toStream match {
                  case Nil    => throw new scala.Error("Impossible happened.")
                  case s :: _ => s.el
                }

              case _ => throw new scala.Error("Impossible happened.")
            }

          } else {
//            println("End of input (in stack)")
            close2()
            go(input)
          }
        case (t1 @ Shifted(_, el1: Ident)) :: t2_ =>
//          println(s"> $t1")
          builder.context.get(el1) match {
            case Some(tr) =>
//              println(">> New segment")
              pushSegment(el1, t1.off)
//              builder.mixfix  = builder.mixfix.map(Some(_)).getOrElse(tr.value)
              builder.mixfix  = tr.value.map(Some(_)).getOrElse(builder.mixfix)
              builder.context = builder.context.copy(tree = tr)
              go(t2_)

            case None =>
              root.get(el1) match {
                case Some(tr) =>
//                  println(">> Root")
                  val context = builder.context
                  pushBuilder(el1, t1.off)
                  builder.mixfix  = tr.value
                  builder.context = Context(tr, Some(context))
                  go(t2_)
                case None =>
//                  println(s"PARENT CHECK (${builder.current.ast}, ${el1})")
                  val currentClosed = builder.context.isEmpty
                  val parentPrecWin = (builder.current.ast, el1) match {
                    case (_: Opr, _) => false
                    case (_, _: Opr) => true
                    case _           => false
                  }
                  val parentBreak = builder.context.parentCheck(el1)
                  (currentClosed || parentPrecWin) && parentBreak match {
                    case true =>
//                      println("Parent close")
                      close2()
                      go(input)
                    case false =>
//                      println(">> Add token")
                      builder.current.revBody ::= t1
                      go(t2_)
                  }
              }
          }
        case t1 :: t2_ =>
          builder.current.revBody ::= t1
          go(t2_)

      }
    }

//      go(Context(registry.tree, None), exprList(t).toList(), List())
//    println("START")
    val elst = exprList(t).toList()
//    pprint.pprintln(elst, width = 50, height = 10000)
    go(elst)
  }

//    def partition(t: AST) = {
//
//      def go(context:Context, lst: SpacedList[AST], out: List[Spaced[AST]]): List[Spaced[AST]]= {
//        context.get(lst.head) match {
//          case None => lst.tail match {
//            case Nil => out
//            case t :: ts => go(context, SpacedList(t,ts), lst.head :: out)
//          }
//          case _ => ???
//        }
//      }
//
//      go(Context(registry.tree, None), exprList(t), List())
//    }

  // format: off


  
  def run(module:Module): Module =
    module.map(_.map(partition))
}