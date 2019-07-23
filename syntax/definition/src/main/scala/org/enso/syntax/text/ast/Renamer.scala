package org.enso.syntax.text.ast

import org.enso.syntax.text.AST_Mod._
import cats.data.NonEmptyList
import javax.swing.tree.MutableTreeNode

import scala.annotation.tailrec

case class Tree[K, V](value: Option[V], branches: Map[K, Tree[K, V]]) {
  def +(item: (List[K], V)): Tree[K, V] = item._1 match {
    case Nil => this.copy(value = Some(item._2))
    case p :: ps => {
      val newBranch = branches.getOrElse(p, Tree[K, V]()) + (ps -> item._2)
      this.copy(branches = branches + (p -> newBranch))
    }
  }

  def get(key: K): Option[Tree[K, V]] =
    branches.get(key)

  def get(path: List[K]): Option[Tree[K, V]] = path match {
    case Nil     => Some(this)
    case p :: ps => branches.get(p).flatMap(_.get(ps))
  }

  def getValue(path: List[K]): Option[V] =
    get(path).flatMap(_.value)

}

object Tree {
  def apply[K, V](): Tree[K, V] = Tree(None, Map())
}

// format: off

object Renamer {
  
  type List1[T] = NonEmptyList[T]
  
  sealed trait Compare
  case object LT extends Compare
  case object GT extends Compare
  case object EQ extends Compare

  def compare[T: Ordering](a: T, b: T): Compare = {
    if (implicitly[Ordering[T]].lt(a, b)) LT
    else if (implicitly[Ordering[T]].gt(a, b)) GT
    else EQ
  }

  /////////////////////////
  //// Mixfix functions////
  /////////////////////////

  
  case class MixfixPattern(
    patterns: NonEmptyList[MixfixPattern.AnySegmentPattern])


  object MixfixPattern {
    def apply(t1:AnySegmentPattern, ts:AnySegmentPattern*): MixfixPattern =
      MixfixPattern(NonEmptyList(t1,ts.to[List]))

    trait SegmentType[T]
    final case class Empty() extends SegmentType[Unit]
    final case class Expr()  extends SegmentType[Option[Spaced[AST]]]
    final case class Expr1() extends SegmentType[Spaced[AST]]

    case class Segment[T](tp: SegmentType[T], head:AST, body:T)
    
    
    case class SegmentPattern[T](head: AST, tp: SegmentType[_])
    type AnySegmentPattern = SegmentPattern[_]

    

    case class Mixfix(segments: SpacedList[Segment[_]])

    
    final case class Registry() {
      var tree = Tree[AST,MixfixPattern]()

      override def toString = tree.toString

      def insert(t: MixfixPattern): Unit =
        tree += t.patterns.toList.map(_.head) -> t

    }

    val registry = Registry()

    registry.insert(
      MixfixPattern(
        SegmentPattern(Operator("("), Expr()),
        SegmentPattern(Operator(")"), Empty())
      )
    )

    registry.insert(
      MixfixPattern(
        SegmentPattern(Var("if"), Expr()),
        SegmentPattern(Var("then"), Expr())
      )
    )

    registry.insert(
      MixfixPattern(
        SegmentPattern(Var("if"), Expr()),
        SegmentPattern(Var("then"), Expr()),
        SegmentPattern(Var("else"), Expr())
      )
    )
    
    case class Context(tree: Tree[AST,MixfixPattern], parent: Option[Context]) {
      def get(t: AST): Option[Tree[AST,MixfixPattern]] =
        tree.get(t)
    }
    
    
    def partition(t: AST) = {

      def go(
          context:Context, 
          lst: List[Spaced[AST]], 
          out: List[Spaced[AST]]
          ): List[Spaced[AST]]= {
        lst match {
          case Nil => println("go end"); out
          case t1 :: t2_ => 
            println(s"> $t1")
            context.get(t1.el) match {
              case None => go(context, t2_, t1 :: out)
              case Some(tr) => 
                val mfx = go2(Context(tr,Some(context)), t2_, None, None, List())
                println(mfx)
                ???
            }
            
        }
      }

      @tailrec
      def go2(
               context : Context,
               lst     : List[Spaced[AST]],
               mixfix  : Option[MixfixPattern],
               current : Option[SpacedList[AST]],
               out     : List[Option[SpacedList[AST]]]
          ): Mixfix = {
        lst match {
          case Nil => 
            println("go2 end")
            val out2 = (current :: out).reverse
            println(current :: out)
            mixfix match {
              case None => ???
              case Some(mfx) =>
                println("!!!")
                val patterns = mfx.patterns.toList.zip(out2)
                val segments = patterns.map { case (pattern, exprs) =>
                  pattern.tp match {
                    case t:Expr  => exprs match {
                      case None => Spaced(0, Segment(t, pattern.head, None))
                      case Some(SpacedList(e,es)) => 
                        println(s">>>> $exprs")
                        val lst = SpacedList(e, es)
                        val ast = Spaced(7,run(lst))
                        Spaced(0, Segment(t, pattern.head, Some(ast)))
                    }
                    case t:Empty => Spaced(0, Segment(t, pattern.head, ()))
                  }
                }
                segments match {
                  case s :: ss => Mixfix(SpacedList(s.el, ss))
                }
            }
          case t1 :: t2_ =>
            println(s">> $t1")
            context.get(t1.el) match {
              case None => current match {
                case None => go2 (context, t2_, mixfix, Some(SpacedList(t1.el,Nil)), out)
                case Some(c) => go2 (context, t2_, mixfix, Some(c.prepend(t1)), out)
              }
              case Some(tr) => 
                val out2 = current :: out
                tr.value match {
                  case None => ???
                  case Some(mfx) => go2(context, t2_, Some(mfx), None, out2)
                }
            }

        }
      }

      go(Context(registry.tree, None), exprList(t).toList(), List())
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
    
    
    
    
  }
  

  //////////////////////////////////////
  //// Spaced / Non-Spaced Segments ////
  //////////////////////////////////////

  case class Spaced[+T](off:Int, el: T) {
    def map[S](f: T => S): Spaced[S] =
      Spaced(off, f(el))
  }

  case class SpacedList[T](head: T, tail: List[Spaced[T]]) {
    def map[S](f: T => S): SpacedList[S] =
      SpacedList(f(head),tail.map(_.map(f)))

    def prepend(t:T, off:Int):SpacedList[T] =
      SpacedList(t, Spaced(off,head) :: tail)

    def prepend(t:Spaced[T]):SpacedList[T] =
      SpacedList(t.el, Spaced(t.off,head) :: tail)
    
    def toList(): List[Spaced[T]] =
      Spaced(0,head) :: tail
  }

  implicit def tupleToSpacedList[T](t:(T,List[Spaced[T]])): SpacedList[T] =
    SpacedList(t._1,t._2)

  type NonSpacedSegment = NonEmptyList[AST]
  type SpacedSegment    = Spaced[NonSpacedSegment]
  type SpacedSegments   = SpacedList[NonSpacedSegment]


  def exprList(ast: AST): SpacedList[AST] = {
    @tailrec
    def go(ast: AST, out: List[Spaced[AST]]): SpacedList[AST] = ast match {
      case App(fn, off, arg) => go(fn, Spaced(off, arg) :: out)
      case ast => SpacedList(ast, out)
    }
    go(ast,List())
  }

  ///////////////////////////////////////////
  //// AST Spaced / Non-Spaced Partition ////
  ///////////////////////////////////////////

  def partitionToSpacedSegments(t: AST): SpacedSegments = {
    @tailrec
    def go(t: AST, stack: List[AST], out: List[SpacedSegment]): SpacedSegments = {
      def currentSeg(t:AST)             = NonEmptyList(t, stack)
      def currentOffSeg(off:Int,t: AST) = Spaced(off,currentSeg(t))
      t match {
        case App(fn, off, arg) => off match {
          case 0 => go (fn, arg :: stack, out)
          case _ => go (fn, Nil, currentOffSeg (off,arg) :: out)
        }
        case _ => SpacedList(currentSeg(t), out)
      }
    }
    go(t, Nil, Nil)
  }

  def partitionToSpacedSegments(t: SpacedList[AST]): SpacedSegments = {
    @tailrec
    def go(t: SpacedList[AST], stack: List[AST], out: List[SpacedSegment]): SpacedSegments = {
      def currentSeg(t:AST)             = NonEmptyList(t, stack)
      def currentOffSeg(off:Int,t: AST) = Spaced(off,currentSeg(t))
      t.tail match {
        case Nil => SpacedList(currentSeg(t.head), out)
        case a :: as => a.off match {
          case 0 => go (SpacedList(a.el,as), t.head :: stack, out)
          case _ => go (SpacedList(a.el,as), Nil, currentOffSeg(a.off,t.head) :: out)
        }
      }
    }
    go(t, Nil, Nil)
  }


  val appOperator: Operator = Operator(" ")

  def astToOp(ast: AST) = ast match {
    case ast: Operator => ast
    case _             => appOperator
  }
  
  ////////////////////////////////////
  //// AST operator-aware rebuild ////
  ////////////////////////////////////
  
  def rebuildOpAwareSubExpr(seg: NonSpacedSegment): AST = {
    rebuildOpAwareExpr(seg) match {
      case t: Section => t.operator
      case t => t
    }
  }

  def rebuildOpAwareExpr(seg: NonSpacedSegment): AST = {
    val sl = SpacedList(seg.head, seg.tail.map(Spaced(0,_)))
    rebuildOpAwareExpr(sl)
  }

  def rebuildOpAwareExpr(seg: SpacedList[AST]): AST = {
    final case class Input(seg: List[Spaced[AST]], stack: SpacedList[AST])
    implicit def input_2(tup: (List[Spaced[AST]], SpacedList[AST])): Input =
      Input(tup._1, tup._2)

    @tailrec
    def go(inp: Input): AST = inp.seg match {
      case Nil => flatten(inp.stack)
      case seg1 :: seg2_ => {

        val shift  = (seg2_, inp.stack.prepend(seg1))
        val reduce = (inp.seg, reduceHead(inp.stack))

        def handleOp(ast1: AST, ast2: AST) = {
          val op1 = astToOp(ast1)
          val op2 = astToOp(ast2)
          compare(op1.prec, op2.prec) match {
            case GT => shift
            case LT => reduce
            case EQ => (op1.assoc, op2.assoc) match {
              case (Left, Left) => reduce
              case _            => shift
            }
          }
        }

        inp.stack.head match {
          case stack1: Operator => seg1.el match {
            case seg1: Operator => go(handleOp(seg1,stack1))
            case _              => go(shift)
          }
          case _ => inp.stack.tail match {
            case Nil         => go(shift)
            case stack2 :: _ => go(handleOp(seg1.el, stack2.el))
          }
        }
      }
    }
    
    go(seg.tail, SpacedList(seg.head, Nil))
  }


  def reduceHead(stack: SpacedList[AST]): SpacedList[AST] = {
    stack.head match {
      case t1: Operator =>
        stack.tail match {
          case Nil => (Section(t1), Nil)
          case t2 :: t3_ => t2.el match {
            case _: Operator => (Section(t1), t2 :: t3_)
            case _           => (SectionRight(t2.el, t2.off, t1), t3_)
          }
        }
      case t1 =>
        stack.tail match {
          case Nil => stack

          case t2 :: t3 :: t4_ => t2.el match {
            case v2: Operator => t3.el match {
              case _:Operator => (SectionLeft(v2, t2.off, t1),t3 :: t4_)
              case _          => (InfixApp(t3.el, t3.off, v2, t2.off, t1),t4_)
            }
            case v2 => (App(v2,t2.off,t1), t3::t4_)
          }

          case t2 :: t3_ => t2.el match {
            case v2: Operator => (SectionLeft(v2, t2.off, t1), t3_)
            case v2           => (App(v2, t2.off, t1), t3_)
          }
        }
    }
  }

  @tailrec
  def flatten(stack: SpacedList[AST]): AST = {
    stack.tail match {
      case Nil => reduceHead(stack).head
      case _   => flatten(reduceHead(stack))
    }
  }
  
  /////////////
  //// API ////
  /////////////

  def run(ast:AST): AST = {
    val segments = partitionToSpacedSegments(ast)
    val flatExpr = segments.map(rebuildOpAwareSubExpr)
    rebuildOpAwareExpr(flatExpr)
  }

  def run(astList:SpacedList[AST]): AST = {
    val segments = partitionToSpacedSegments(astList)
    val flatExpr = segments.map(rebuildOpAwareSubExpr)
    rebuildOpAwareExpr(flatExpr)
  }
  
  
//  def run(mfx:Mixfix): AST = {
//    mfx.
//  }
  
  def run(module:Module): Module =
    module.map(_.map(run))
}