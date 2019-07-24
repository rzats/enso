package org.enso.syntax.text.ast

import org.enso.syntax.text.AST._
import org.enso.syntax.text._
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

  ////////////////
  //// Mixfix ////
  ////////////////




  


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

      case class SegmentBuilder(
        leftOff  : Int, 
        div      : AST, 
        rightOff : Int, 
        body     : SpacedList[AST]
      )
      
      
      class MixfixBuilder() {
        var context : Context                               = Context()
        var mixfix  : Option[Mixfix.Header]                 = None
        var current : Option[Spaced[SpacedList[AST]]]       = None
        var out     : List[Option[Spaced[SpacedList[AST]]]] = List()
      }
      
      var builder: MixfixBuilder = new MixfixBuilder()
      builder.mixfix = Some(Mixfix.Header(NonEmptyList(Mixfix.Segment.Expr(Var("module")),Nil)))
      var builderStack: List[(MixfixBuilder,Int)] = Nil
      
      def pushBuilder(off: Int): Unit = {
        println("pushBuilder")
        builderStack +:= (builder, off) 
        builder = new MixfixBuilder()
      }
      
      def popBuilder(): Int = {
        println("popBuilder")
        val (bldr,off) = builderStack.head
        builder = bldr
        builderStack = builderStack.tail
        off
      }
      
      def getLastBuilderOffset(): Int = {
        val (bldr,off) = builderStack.head
        off
      }
      

      import Mixfix._
      
      
      @tailrec
      def go(root : Context, input: List[Spaced[AST]]): List[Spaced[AST]] = {
        
        def close(goff:Int): List[Spaced[AST]] = {
          println(s"\n\n-----------------------------------\nclose($goff)")
          val out2 = builder.current :: builder.out
          println(s"out2 =")
          pprint.pprintln(out2,width = 50,height = 10000)
          val result = builder.mixfix match {
            case None => ???
            case Some(mfx) =>
              val revPatterns = mfx.segments.toList.reverse.zip(out2)
              val lastPattern = revPatterns.head
              val segments = revPatterns.reverseMap { case (pattern, exprs) =>
                pattern.tp match {
                  case t: Segment.Type.Expr => exprs match {
                    case None => Spaced(-1, Segment(t, pattern.head, None))
                    case Some(Spaced(off,SpacedList(e,es))) =>
                      val es2 = es match {
                        case Nil => Nil
                        case e :: ee => Spaced(off, e.el) :: ee
                      }
                      println(s"OFF: $off")
                      println(s"E: $e")
                      println(s"ES: $es")
                      val lst = SpacedList(e, es2)
                      println("Rebuilding:")
                      pprint.pprintln(lst,width = 50,height = 10000)
                      val ast = Spaced(off,run(lst))
                      Spaced(-11, Segment(t, pattern.head, Some(ast)))
                  }
                  case t: Segment.Type.Empty =>
                    Spaced(-2, Segment(t, pattern.head, ()))
                }
              }


              val mx = segments match {
                case s :: ss => Mixfix(SpacedList(s.el, ss))
              }

              val suffix: List[Spaced[AST]] = lastPattern match { case (pattern, exprs) =>
                pattern.tp match {
                  case t: Segment.Type.Empty => exprs match {
                    case None => List()
                    case Some(Spaced(s,e)) => Spaced(s,e.head) :: e.tail
                  }
                  case _ => List()
                }
              }
              suffix :+ Spaced(goff,mx)
          }
          println("Close Result:")
          pprint.pprintln(result,width = 50,height = 10000)
          result
        }
        
        def close2() = {
          println("close2")
          val off = getLastBuilderOffset()
          val lst = close(off)
          popBuilder()
          //          println("@@@", off, lst)
//          println(builder.current)
          builder.current = builder.current match {
            case None =>
              Some(Spaced(lst.head.off,SpacedList(lst.head.el, lst.tail)))
            case Some(Spaced(o,s)) => 
//              println("-----------")
//              println(o)
//              pprint.pprintln(s,width = 50,height = 10000)
//              pprint.pprintln(lst, width = 50, height = 10000)
//              Some(Spaced(o,s + lst))
              val head = lst.head
              val tail = lst.tail
              val tail2 = Spaced(o,s.head) :: s.tail
              Some(Spaced(head.off, SpacedList(head.el, tail ++ tail2)))
          }
        }
        
        input match {
          case Nil => 
            if (builderStack.isEmpty) {
              println("End of input (not in stack)")
              close(-4)
            }
            else {
              println("End of input (in stack)")
              close2()
              go(root, input)
            }
          case t1 :: t2_ =>
            println(s"> $t1")
            
            builder.context.parentCheck(t1.el) match {
              case true =>
                close2()
                go(root, input)
              case false => root.get(t1.el) match {
                case Some(tr) =>
                  println(">> Root")
                  val context = builder.context
                  pushBuilder(t1.off)
                  builder.context = Context(tr, Some(context))
                  go(root, t2_)
                case None => builder.context.get(t1.el) match {
                  case None => builder.current match {
                    case None =>
                      println(">> First token")
                      builder.current = Some(Spaced(t1.off, SpacedList(t1.el, Nil)))
                      go(root, t2_)
                    case Some(c) =>
                      println(">> Non-First token")
                      builder.current = Some(c.map(_.prepend(t1)))
                      go(root, t2_)
                  }
                  case Some(tr) =>
                    println(">> New segment")
                    val out2 = builder.current :: builder.out
                    tr.value match {
                      case None => ???
                      case Some(mfx) =>
//                        builder.segOff = t1.off
                        builder.mixfix = Some(mfx)
                        builder.current = None
                        builder.out = out2
                        go(root, t2_)
                    }
                }
              }
            }

        }
      }

//      go(Context(registry.tree, None), exprList(t).toList(), List())
      go(Context(registry.tree, None), exprList(t).toList())
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
  
  
//  def run(mfx:Mixfix): AST = {
//    mfx.
//  }
  
  def run(module:Module): Module =
    module.map(_.map(run))
}