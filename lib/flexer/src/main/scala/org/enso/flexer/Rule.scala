package org.enso.flexer

import scala.reflect.runtime.universe.{Expr, Tree}

class Rule(val expr: Pattern, var fn: Tree) {

  def run(f: Expr[_]): Unit = {
    fn = f.tree
  }
}
