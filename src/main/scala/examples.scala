package arithtree.examples

import arithtree.tree._

object Examples {
  val examples: List[Tree] = List(
    Plus(Var("x"), Const(0)),

    Plus(Const(0), Var("x")),

    Minus(Var("x"), Const(0)),

    Minus(Var("x"), Var("x")),

    Plus(Var("x"), Minus(Const(0), Var("x"))),

    Times(Var("x"), Const(0)),

    Times(Const(0), Var("x")),

    Times(Var("x"), Const(1)),

    Times(Const(1), Var("x")),

    Div(Const(0), Var("x")),

    Div(Var("x"), Const(1)),

    Div(Var("x"), Var("x")),

    Plus(Minus(Const(1), Var("a")), Const(3)),

    Times(Plus(Var("x"), Var("y")),
          Div(Plus(Const(3), Var("x")),
              Const(4))),

    Div(Plus(Var("a"), Var("b")),
        Minus(Var("a"), Var("b")))
  )
}
