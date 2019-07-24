package org.enso.syntax.text.ast

import org.enso.syntax.text.AST._
import org.enso.syntax.text._
import cats.data.NonEmptyList
import javax.swing.tree.MutableTreeNode

import scala.annotation.tailrec

// format: off

object Renamer {
  
  
  sealed trait Compare
  case object LT extends Compare
  case object GT extends Compare
  case object EQ extends Compare

  def compare[T: Ordering](a: T, b: T): Compare = {
    if (implicitly[Ordering[T]].lt(a, b)) LT
    else if (implicitly[Ordering[T]].gt(a, b)) GT
    else EQ
  }

  ////////////////
  //// Mixfix ////
  ////////////////




  
  implicit def tupleToList1[T](t:(T,List[T])): List1[T] =
    List1(t._1,t._2)
  
  type List1[T] = NonEmptyList[T]
  val List1 = NonEmptyList

  object MMM {
    
    


    
    final case class Registry() {
      var tree = Tree[AST,Mixfix.Header]()

      override def toString(): String = 
        tree.toString

      def insert(t: Mixfix.Header): Unit =
        tree += t.segments.toList.map(_.head) -> t
    }
    object Registry {
      def apply(ts: Mixfix.Header*): Registry = {
        val registry = new Registry()
        ts.foreach(registry.insert)
        registry
      }
    }

    val registry = Registry(
      Mixfix.Header(
        Mixfix.Segment.Expr(Operator("(")),
        Mixfix.Segment.Empty(Operator(")"))
      ),
      Mixfix.Header(
        Mixfix.Segment.Expr(Var("if")),
        Mixfix.Segment.Expr(Var("then"))
      )//,
//      Mixfix.Header(
//        Mixfix.Segment.Expr(Var("if")),
//        Mixfix.Segment.Expr(Var("then")),
//        Mixfix.Segment.Expr(Var("else"))
//      )
    )
    
    case class Context(tree: Tree[AST,Mixfix.Header], parent: Option[Context]) {
      def get(t: AST): Option[Tree[AST,Mixfix.Header]] =
        tree.get(t)
      
      @tailrec
      final def parentCheck(t: AST): Boolean = parent match {
        case None => false
        case Some(p) => p.get(t) match {
          case None    => p.parentCheck(t)
          case Some(_) => true
        }
      }
    }
    object Context {
      def apply(): Context = Context(Tree(), None)
    }
    
    
    def partition(t: AST) = {

      class SegmentBuilder() {
        var offset: Int = 0
        //        div      : AST,
        //        rightOff : Int,
        var revBody: List[Spaced[AST]] = List()

        override def toString(): String = 
          s"SegmentBuilder($offset, $revBody)"
      }
      
      
      class MixfixBuilder() {
        var context     : Context               = Context()
        var mixfix      : Option[Mixfix.Header] = None
        var current     : SegmentBuilder        = new SegmentBuilder()
        var revSegments : List[SegmentBuilder]  = List()
      }
      
      var builder: MixfixBuilder = new MixfixBuilder()
      builder.mixfix = Some(
        Mixfix.Header(List1(Mixfix.Segment.Expr(Var("module")),Nil)))
      var builderStack: List[MixfixBuilder] = Nil
      
      def pushBuilder(off: Int): Unit = {
        println(s"pushBuilder($off)")
        builderStack +:= builder 
        builder = new MixfixBuilder()
        builder.current.offset = off
      }
      
      def popBuilder(): Unit = {
        println("popBuilder")
        builder = builderStack.head
        builderStack = builderStack.tail
      }
      
      def pushSegment(off: Int): Unit = {
        println(s"pushSegment($off)")
        builder.revSegments ::= builder.current
        builder.current = new SegmentBuilder()
        builder.current.offset = off
      }
      
      def getLastBuilderOffset(): Int = {
//        val (bldr,off) = builderStack.head
//        off
        -999
      }
      

      import Mixfix._
      
      
      val root = Context(registry.tree, None)
      
      @tailrec
      def go(input: List[Spaced[AST]]): List[Spaced[AST]] = {
        
        def close(): List[Spaced[AST]] = {
          println(s"\n\n-----------------------------------\n\n")
          val revSegments = builder.current :: builder.revSegments
          println(s"revSegments =")
          pprint.pprintln(revSegments,width = 50,height = 10000)
          val result = builder.mixfix match {
            case None => 
              println(builder.context.tree.dropValues())
              ???
            case Some(mfx) =>
              val revPatterns = mfx.segments.toList.reverse
              val revSegDefs  = revPatterns.zip(revSegments)
              val lastPattern = revSegDefs.head
              val segments    = revSegDefs.reverseMap { case (pattern, segBldr) =>
                pattern.tp match {
                  case t: Segment.Type.Expr => segBldr.revBody match {
                    case seg1 :: seg2_ =>
                      val Spaced(o,tt) = partitionToSpacedSegments2(List1(seg1, seg2_))
                      println("TT")
                      pprint.pprintln(tt,width = 50,height = 10000)
                      val ast = Spaced(o, runx(tt))
                      Spaced(segBldr.offset, Segment(t, pattern.head, Some(ast)))
                  }
                    case t: Segment.Type.Empty =>
                      Spaced(segBldr.offset, Segment(t, pattern.head, ()))
                  
                }
              }


              val mx = segments match {
                case s :: ss => Spaced(s.off, Mixfix(SpacedList(s.el, ss)))
              }

              val suffix: List[Spaced[AST]] = lastPattern match { case (pattern, segBldr) =>
                pattern.tp match {
                  case t: Segment.Type.Empty => segBldr.revBody
                  case _ => List()
                }
              }
              suffix :+ mx
          }
          println("Close Result:")
          pprint.pprintln(result,width = 50,height = 10000)
          result
        }
        
        def close2() = {
          println("close2")
          val subAst = close()
          popBuilder()
          builder.current.revBody = subAst ++ builder.current.revBody
        }
        
        input match {
          case Nil => 
            if (builderStack.isEmpty) {
              println("End of input (not in stack)")
              close()
            }
            else {
              println("End of input (in stack)")
              close2()
              go(input)
            }
          case t1 :: t2_ =>
            println(s"> $t1")

            builder.context.get(t1.el) match {
              case Some(tr) =>
                println(">> New segment")
                pushSegment(t1.off)
                builder.mixfix  = builder.mixfix.map(Some(_)).getOrElse(tr.value)
                builder.context = builder.context.copy(tree = tr)
                go(t2_)
                
              case None => builder.context.parentCheck(t1.el) match {
                case true =>
                  println("Parent close")
                  close2()
                  go(input)
                case false => root.get(t1.el) match {
                  case Some(tr) =>
                    println(">> Root")
                    val context = builder.context
                    pushBuilder(t1.off)
                    builder.context = Context(tr, Some(context))
                    go(t2_)
                  case None =>
                    println(">> Add token")
                    builder.current.revBody ::= t1
                    go(t2_)
                }
              }
            }

          
            
            
            

        }
      }

//      go(Context(registry.tree, None), exprList(t).toList(), List())
      println("START")
      val elst = exprList(t).toList()
      pprint.pprintln(elst,width = 50,height = 10000)
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
    
    
    
    
  }
  

  //////////////////////////////////////
  //// Spaced / Non-Spaced Segments ////
  //////////////////////////////////////



  type NonSpacedSegment = List1[AST]
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
      def currentSeg(t:AST)             = List1(t, stack)
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
      def currentSeg(t:AST)             = List1(t, stack)
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

  def partitionToSpacedSegments2
    (t: List1[Spaced[AST]]): Spaced[SpacedList[NonSpacedSegment]] = {
    @tailrec
    def go(
      input   : List[Spaced[AST]], 
      lastOff : Int,
      current : List1[AST], 
      out     : List[Spaced[NonSpacedSegment]]
    ): Spaced[SpacedList[NonSpacedSegment]] = {
      input match {
        case Nil => Spaced(lastOff, SpacedList(current, out))
        case s :: ss => lastOff match {
          case 0 => go(ss, s.off, s.el :: current, out)
          case i => go(ss, s.off, (s.el, Nil), Spaced(i,current) :: out)
        }
      }
    }
    go(t.tail, t.head.off, (t.head.el,Nil), Nil)
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
      case t: App.Sides => t.operator
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
          case Nil => (App.Sides(t1), Nil)
          case t2 :: t3_ => t2.el match {
            case _: Operator => (App.Sides(t1), t2 :: t3_)
            case _           => (App.Left(t2.el, t2.off, t1), t3_)
          }
        }
      case t1 =>
        stack.tail match {
          case Nil => stack

          case t2 :: t3 :: t4_ => t2.el match {
            case v2: Operator => t3.el match {
              case _:Operator => (App.Right(v2, t2.off, t1),t3 :: t4_)
              case _          => (App.Infix(t3.el, t3.off, v2, t2.off, t1),t4_)
            }
            case v2 => (App(v2,t2.off,t1), t3::t4_)
          }

          case t2 :: t3_ => t2.el match {
            case v2: Operator => (App.Right(v2, t2.off, t1), t3_)
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

  def runx(segments:SpacedSegments): AST = {
    val flatExpr = segments.map(rebuildOpAwareSubExpr)
    rebuildOpAwareExpr(flatExpr)
  }
  
  
//  def run(mfx:Mixfix): AST = {
//    mfx.
//  }
  
  def run(module:Module): Module =
    module.map(_.map(run))
}