package org.enso.syntax.text.ast

import org.enso.syntax.text.AST_Mod._
import cats.data.NonEmptyList
import scala.annotation.tailrec

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

  def cross(as: List[String], bs: List[String]) =
    for { a <- as; b <- bs } yield a + b



  //////////////////////////////////////
  //// Spaced / Non-Spaced Segments ////
  //////////////////////////////////////

  //// Definition ////

  case class Spaced[T](off:Int, el: T) {
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
  }

  implicit def tupleToSpacedList[T](t:(T,List[Spaced[T]])): SpacedList[T] =
    SpacedList(t._1,t._2)

  type NonSpacedSegment = NonEmptyList[AST]
  type SpacedSegment    = Spaced[NonSpacedSegment]
  type SpacedSegments   = SpacedList[NonSpacedSegment]

  //// API ////

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


  val appOperator: Operator = Operator(" ")

  def astToOp(ast: AST) = ast match {
    case ast: Operator => ast
    case _             => appOperator
  }

  
  def rebuildAssocSubExpr(seg: NonSpacedSegment): AST = {
    rebuildAssocExpr(seg) match {
      case t: Section => t.operator
      case t => t
    }
  }

  def rebuildAssocExpr(seg: NonSpacedSegment): AST = {
    val sl = SpacedList(seg.head, seg.tail.map(Spaced(0,_)))
    rebuildAssocExpr(sl)
  }

  def rebuildAssocExpr(seg: SpacedList[AST]): AST = {
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



  def run(ast:AST):AST = {
    val segments = partitionToSpacedSegments(ast)
    val flatExpr = segments.map(rebuildAssocSubExpr)
    rebuildAssocExpr(flatExpr)
  }


  def run(module:Module): Module =
    module.map(_.map(run))



}
