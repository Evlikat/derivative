package sctree

/**
  * Created by Роман on 21.02.2016.
  */
abstract class Tree {

  type Args = String => Double

  def eval(args: Args) : Double = this match {
    case Zero => 0
    case One => 1
    case Var(name) => args(name)
    case Const(value) => value
    case Sum(l, r) => l.eval(args) + r.eval(args)
    case Mul(l, r) => l.eval(args) * r.eval(args)
  }

  def eval() : Double = eval(Tree.withNoArgs)

  def d(x: String) : Tree = this match {
    case Zero | One => Zero
    case Const(value) => Zero
    case Var(name) if name != x => Zero
    case Var(name) if name == x => One
    case Sum(l, r) => l.d(x) + r.d(x)
    case Mul(l, r) => l.d(x) * r + l * r.d(x)
  }

  def +(other: Tree) : Tree = (this, other) match {
    case (Zero, _) | (Const(0), _) => other
    case (_, Zero) | (_, Const(0)) => this
    case (Const(value), Const(otherValue)) => Const(value + otherValue)
    case (Var(name), Var(otherName)) if name == otherName => Mul(Const(2), Var(name))
    case (Var(name), Var(otherName)) if name != otherName => Sum(this, other)
    case _ => new Sum(this, other)
  }

  def ?(name: String) : Tree = this.+(new Var(name))
  def +(name: String) : Tree = this.+(new Var(name))
  def ?(value: Double) : Tree = this.+(new Const(value))
  def +(value: Double) : Tree = this.+(new Const(value))

  def *(other: Tree) : Tree = (this, other) match {
    case (Zero, _) | (_, Zero) | (Const(0), _) | (_, Const(0)) => Zero
    case (One, _) | (Const(1), _) => other
    case (_, One) | (_, Const(1)) => this
    case (Const(value), Const(otherValue)) => Const(value * otherValue)
    case _ => Mul(this, other)
  }

  def *(name: String) : Tree = this.*(new Var(name))
  def *(value: Double) : Tree = this.*(new Const(value))
}

object Tree {

  def exp() = Zero
  val withNoArgs: (String => Double) = null
}

case object Zero extends Tree
case object One extends Tree
case class Var(name: String) extends Tree
case class Const(value: Double) extends Tree
case class Sum(l: Tree, r: Tree) extends Tree
case class Mul(l: Tree, r: Tree) extends Tree