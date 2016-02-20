package scalatree

/**
  * Created by rprokhorov on 2/20/16.
  */
abstract class Tree {

  type Arguments = String => Double

  def eval(args: Arguments) : Double = this match {
    case Zero => 0
    case One => 1
    case Const(v) => v
    case Var(name) => args.apply(name)
    case Sum(l, r) => l.eval(args) + r.eval(args)
    case Mul(l, r) => l.eval(args) * r.eval(args)
  }

  def derivative() : Tree = this match {
    case Const(v) => Zero
    case Var(name) => One
    case Sum(l, r) => new Sum(l.derivative(), r.derivative())
    case Mul(l, r) => new Sum(new Mul(l.derivative(), r), new Mul(l, r.derivative()))
  }

  def +(other: Tree) : Tree = this match {
    case Zero => other
    case _ => other match {
      case Zero => this
      case _ => new Sum(this, other)
    }
  }

  def *(other: Tree) : Tree = this match {
    case Zero => Zero
    case One => other
    case _ => other match {
      case Zero => Zero
      case One => this
      case _ => new Mul(this, other)
    }
  }
}

object Tree {

  val withNoArgs: (String => Double) = null

  implicit class ConstTree(i: Double) {
    implicit def toTree(i: Double) : Tree = new Const(i)
  }
}

case object Zero extends Tree
case object One extends Tree

case class Const(value: Double) extends Tree
case class Var(name: String) extends Tree

case class Sum(l: Tree, r: Tree) extends Tree
case class Mul(l: Tree, r: Tree) extends Tree