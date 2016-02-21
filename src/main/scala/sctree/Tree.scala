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

  def d(x: String) : Tree = this match {
    case Zero => Zero
    case One => Zero
    case Var(name) => if (name == x) One else Zero
    case Const(value) => Zero
    case Sum(l, r) => l.d(x) + r.d(x)
    case Mul(l, r) => l.d(x) * r + l * r.d(x)
  }

  def +(other: Tree) : Tree = this match {
    case Zero => other
    case Const(value) => other match {
      case Const(otherValue) => Const(value + otherValue)
      case _ => other match {
        case Zero => this
        case _ => new Sum(this, other)
      }
    }
    case Var(name) => other match {
      case Var(otherName) => if (name == otherName) Mul(Const(2), Var(name)) else Sum(this, other)
      case _ => other match {
        case Zero => this
        case _ => new Sum(this, other)
      }
    }
    case _ => other match {
      case Zero => this
      case _ => new Sum(this, other)
    }
  }

  def ?(name: String) : Tree = this.+(new Var(name))
  def +(name: String) : Tree = this.+(new Var(name))
  def ?(value: Double) : Tree = this.+(new Const(value))
  def +(value: Double) : Tree = this.+(new Const(value))

  def *(other: Tree) : Tree = this match {
    case Zero => Zero
    case One => other
    case Const(value) => other match {
      case Const(otherValue) => Const(value * otherValue)
      case _ => other match {
        case Zero => Zero
        case One => this
        case _ => Mul(this, other)
      }
    }
    case _ => other match {
      case Zero => Zero
      case One => this
      case _ => Mul(this, other)
    }
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