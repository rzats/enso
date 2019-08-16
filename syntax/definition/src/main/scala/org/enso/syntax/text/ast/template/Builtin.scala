package org.enso.syntax.text.ast.template

import org.enso.data.List1
import org.enso.data.Shifted
import org.enso.syntax.text.AST
import org.enso.syntax.text.AST.Template.Definition
import org.enso.syntax.text.AST.Cons
import org.enso.syntax.text.AST.Opr
import org.enso.syntax.text.AST.Var

object Builtin {

  val registry: Registry = {

    def internalError = throw new Error("Internal error")

    val groupDef = Definition(
      Opr("(") -> Pattern.Opt(Pattern.Expr()),
      Opr(")") -> Pattern.Nothing()
    ) {
      case List(st1, _) =>
        st1.body.toStream match {
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

    val importDef = Definition(
      Var("import") ->
      Pattern.SepList(Pattern.Cls[Cons], AST.Opr("."), "expected module name")
    ) {
      case List(s1) =>
        import Pattern.Match._
        s1.body match {
          case Seq(headMatch, Many(tailMatch)) =>
            def unwrapSeg(lseg: Pattern.Match): Cons =
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
      case _ => internalError
    }

    val ifThenDef = Definition(
      Var("if")   -> Pattern.Expr(),
      Var("then") -> Pattern.Expr()
    ) {
      case List(s1, s2) =>
        (s1.body.toStream, s2.body.toStream) match {
          case (List(t1), List(t2)) =>
            AST.Mixfix(List1(s1.head, s2.head), List1(t1.el, t2.el))
          case _ => internalError
        }
      case _ => internalError
    }

    val ifThenElseDef = Definition(
      Var("if")   -> Pattern.Expr(),
      Var("then") -> Pattern.Expr(),
      Var("else") -> Pattern.Expr()
    ) {
      case List(s1, s2, s3) =>
        (s1.body.toStream, s2.body.toStream, s3.body.toStream) match {
          case (List(t1), List(t2), List(t3)) =>
            AST.Mixfix(
              List1(s1.head, s2.head, s3.head),
              List1(t1.el, t2.el, t3.el)
            )
          case _ => internalError
        }
      case _ => internalError
    }

    Registry(
      groupDef,
      ifThenDef,
      ifThenElseDef,
      importDef
      //      defDef
    )
  }

}
