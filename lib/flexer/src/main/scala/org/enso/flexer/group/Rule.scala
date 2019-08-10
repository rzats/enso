package org.enso.flexer.group

import org.enso.flexer.Pattern

import scala.reflect.runtime.universe.Expr
import scala.reflect.runtime.universe.Tree

final case class Rule(pattern: Pattern, tree: Tree)
object Rule {
  final case class Builder(pattern: Pattern, finalizer: Rule => Unit) {
    def run(expr: Expr[_]): Unit = run(expr.tree)
    def run(tree: Tree):    Unit = finalizer(Rule(pattern, tree))
  }
}
