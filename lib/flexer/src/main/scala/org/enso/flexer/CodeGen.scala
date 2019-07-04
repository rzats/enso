package org.enso.flexer

import scala.annotation.tailrec
import scala.reflect.runtime.universe._

case class CodeGen(dfa: DFA) {

  var code = q""

  case class Branch(rangeEnd: Int, target: BranchTarget)
  case class BranchTarget(state: Int, code: Option[Tree])

  def cleanBranches(revInput: List[Branch]): List[Branch] = {
    @tailrec def go(rIn: List[Branch], out: List[Branch]): List[Branch] =
      rIn match {
        case Nil => out
        case i :: is =>
          out match {
            case Nil => go(is, i :: Nil)
            case o :: os =>
              if (o.target == i.target) go(is, out)
              else go(is, i :: out)
          }
      }
    go(revInput, Nil)
  }

  def genBranches(branches: List[Branch]): Tree = {
    branches.foldLeft(identity[Tree] _) {
      case (ifBlock, branch) =>
        elseBody =>
          val body = branch.target match {
            case BranchTarget(-1, None)        => q"-2"
            case BranchTarget(-1, Some(block)) => q"{..$block; -1}"
            case BranchTarget(st, _)           => q"${Literal(Constant(st))}"
          }
          ifBlock(q"if (codePoint <= ${branch.rangeEnd}) $body else $elseBody")
    }(q"-2")
  }

  def generateStateMatch(): Tree = {
    val branches = dfa.links.indices.toList.map { state =>
      var revBranches: List[Branch] = Nil
      for ((range, vocIx) <- dfa.vocabulary.iter) {
        val targetState  = dfa.links(state)(vocIx)
        val p1           = dfa.endStatePriorityMap.get(state)
        val branchTarget = BranchTarget(targetState, p1.map(_.code))
        revBranches ::= Branch(range.end, branchTarget)
      }

      val branches = cleanBranches(revBranches)

      cq"$state => ${genBranches(branches)}"
    }
    Match(q"state", branches)
  }

  def generate(i: Int): Tree = {
    code = q"""
            ..$code;
            def ${TermName(s"runGroup$i")}(): Int = {
              var state: Int = 0
              matchBuilder.setLength(0)
              while(state >= 0) {
                codePoint = currentChar.toInt
                state = ${generateStateMatch()}
                if(state >= 0) {
                  matchBuilder.append(currentChar)
                  currentChar = getNextChar
                }
              }
              state
            }
         """
    code
  }
}
