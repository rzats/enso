package org.enso.flexer

import org.enso.flexer.group.Rule
import scala.reflect.runtime.universe.TermName
import scala.reflect.runtime.universe.Tree

class Group(val groupIx: Int, val finish: () => Unit) {
  var parent: Option[Group] = None
  var revRules: List[Rule]  = List()

  def parent_=(p: Group): Unit =
    parent = Some(p)

  def addRule(rule: Rule): Unit =
    revRules = rule +: revRules

  def rule(expr: Pattern): Rule.Builder =
    Rule.Builder(expr, addRule)

  def cloneRulesFrom(that: Group): Unit =
    revRules = that.revRules ++ revRules

  private def ruleName(ruleIx: Int): TermName =
    TermName(s"group${groupIx}_rule$ruleIx")

  def allRules: List[Rule] = {
    val myRules = revRules.reverse
    parent.map(myRules ++ _.allRules).getOrElse(myRules)
  }

  def buildAutomata(): NFA = {
    val nfa   = new NFA
    val start = nfa.addState()
    val endpoints = allRules.zipWithIndex.map {
      case (rule, ix) => buildRuleAutomata(nfa, start, ix, rule)
    }
    val end = nfa.addState()
    nfa.state(end).end = true
    for (endpoint <- endpoints) {
      nfa.link(endpoint, end)
    }
    nfa
  }

  def buildRuleAutomata(nfa: NFA, last: Int, ruleIx: Int, rule: Rule): Int = {
    val end = buildExprAutomata(nfa, last, rule.pattern)
    nfa.state(end).end  = true
    nfa.state(end).rule = ruleName(ruleIx)
    end
  }

  def buildExprAutomata(nfa: NFA, last: Int, expr: Pattern): Int = {
    val current = nfa.addState()
    nfa.link(last, current)
    expr match {
      case None_ => nfa.addState()
      case Pass  => current
      case Ran(start, end) =>
        val state = nfa.addState()
        nfa.link(current, state, start, end)
        state
      case Seq_(first, second) =>
        val s1 = buildExprAutomata(nfa, current, first)
        buildExprAutomata(nfa, s1, second)
      case Many(body) =>
        val s1 = nfa.addState()
        val s2 = buildExprAutomata(nfa, s1, body)
        val s3 = nfa.addState()
        nfa.link(current, s1)
        nfa.link(current, s3)
        nfa.link(s2, s3)
        nfa.link(s3, s1)
        s3
      case Or(first, second) =>
        val s1 = buildExprAutomata(nfa, current, first)
        val s2 = buildExprAutomata(nfa, current, second)
        val s3 = nfa.addState()
        nfa.link(s1, s3)
        nfa.link(s2, s3)
        s3
    }
  }

  def generate(): Tree = {
    import scala.reflect.runtime.universe._
    val nfa = buildAutomata()
    nfa.computeIsos()
    val dfa   = nfa.computeDFA()
    val state = CodeGen(dfa).generate(groupIx)
    val rules = allRules.zipWithIndex.map {
      case (rule, ruleIx) => q"def ${ruleName(ruleIx)}() = ${rule.tree}"
    }
    q"..$state; ..$rules"
  }
}
