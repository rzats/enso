package org.enso.flexer.group

import org.enso.flexer.Pattern

import scala.reflect.runtime.universe.Expr
import scala.reflect.runtime.universe.Tree

final case class Rule(expr: Pattern, tree: Tree)
object Rule {
  final case class Builder(expr: Pattern, finalizer: Rule => Unit) {
    def run(expr: Expr[_]): Unit = run(expr.tree)
    def run(tree: Tree):    Unit = finalizer(Rule(expr, tree))
  }
}
