package arithtree.tree

object TreeOps {
  type Environment = Tree => Tree

  def eval(t: Tree, env: Environment): Tree = t match {
    case Plus(l, r)  => add(eval(l, env), eval(r, env))
    case Minus(l, r) => sub(eval(l, env), eval(r, env))
    case Times(l, r) => mul(eval(l, env), eval(r, env))
    case Div(l, r)   => div(eval(l, env), eval(r, env))
    case v: Var      => env(v)
    case other       => other
  }

  def add(left: Tree, right: Tree): Tree = (left, right) match {
    case (Const(l), Const(r)) => Const(l+r)
    case (anything, Const(0)) => anything
    case (Const(0), anything) => anything
    case (l, Minus(Const(0), r)) if (l == r) => Const(0)
    case other                => Plus(left, right)
  }

  def sub(left: Tree, right: Tree): Tree = (left, right) match {
    case (Const(l), Const(r)) => Const(l-r)
    case (anything, Const(0)) => anything
    case (l, r) if (l == r)   => Const(0)
    case other                => Minus(left, right)
  }

  def mul(left: Tree, right: Tree): Tree = (left, right) match {
    case (Const(l), Const(r)) => Const(l*r)
    case (anything, Const(0)) => Const(0)
    case (Const(0), anything) => Const(0)
    case (anything, Const(1)) => anything
    case (Const(1), anything) => anything
    case other                => Times(left, right)
  }

  def div(left: Tree, right: Tree): Tree = (left, right) match {
    case (Const(l), Const(r)) => Const(l/r)
    case (Const(0), anything) => Const(0)
    case (anything, Const(1)) => anything
    case (l, r) if (l == r)   => Const(1)
    case other                => Div(left, right)
  }

  def env(bindings: Iterator[Tuple2[Tree, Tree]]): Environment = {
    val m = bindings.toMap
    (t: Tree) => { m.getOrElse(t, t) }
  }
}

abstract class Tree
abstract class Unary(v: Any) extends Tree {
  override def toString: String = "" + v
}
abstract class Binary(l: Tree, r: Tree) extends Tree {
  val sym: String

  private def wrapParens(t: Tree): String = t match {
    case x: Unary  => x.toString
    case x: Binary => "(" + x.toString + ")"
  }

  override def toString: String = {
    wrapParens(l) + sym + wrapParens(r)
  }
}
case class Plus(l: Tree, r: Tree) extends Binary(l, r) {
  val sym = " + "
}
case class Minus(l: Tree, r: Tree) extends Binary(l, r) {
  val sym = " - "
}
case class Times(l: Tree, r: Tree) extends Binary(l, r) {
  val sym = " * "
}
case class Div(l: Tree, r: Tree) extends Binary(l, r) {
  val sym = " / "
}
case class Var(n: String) extends Unary(n)
case class Const(v: Int) extends Unary(v)

