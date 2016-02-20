package tree

/**
  * Created by rprokhorov on 2/20/16.
  */

abstract class Tree {

  type Arguments = String => Double

  def eval(args: Arguments): Double

  def d(targetVariable: String): Tree

  def +(another: Tree) : Tree = another match {
    case Zero => this
    case _ => new Sum(this, another)
  }

  def +(value: Double) : Tree = value match {
    case 0 => this
    case _ => new Sum(this, Const.by(value))
  }

  def +(name: String) : Tree = new Sum(this, new Var(name))

  def *(another: Tree) : Tree = another match {
    case Zero => Zero
    case One => this
    case _ => new Mul(this, another)
  }

  def *(value: Double) : Tree = value match {
    case 0 => Zero
    case 1 => this
    case _ => new Mul(this, Const.by(value))
  }

  def *(name: String) : Tree = new Mul(this, new Var(name))
}

object Tree {

  def exp(value: Double) = Const.by(value)

  def exp() = Zero

  def exp(name: String) = new Var(name)

  val withNoArgs: (String => Double) = null
}

class Sum(l: Tree, r : Tree) extends Tree {

  override def eval(args: Arguments) : Double = l.eval(args) + r.eval(args)

  override def d(targetVariable: String): Tree = l.d(targetVariable) + r.d(targetVariable)
}

class Mul(l: Tree, r: Tree) extends Tree {

  override def eval(args: Arguments) : Double = l.eval(args) * r.eval(args)

  override def d(x: String): Tree = (l.d(x) * r) + (l * r.d(x))
}

class Var(name: String) extends Tree {

  override def eval(args: Arguments) : Double = args.apply(name)

  override def d(targetVariable: String): Tree = if (name == targetVariable) One else Zero
}

class Const private(c: Double) extends Tree {

  override def eval(args: Arguments) : Double = c

  override def d(targetVariable: String): Tree = Zero
}

object Const {

  def by(value: Double) = value match {
    case 0 => Zero
    case 1 => One
    case _ => new Const(value)
  }
}

object Zero extends Tree {

  override def eval(args: Arguments): Double = 0

  override def d(targetVariable: String): Tree = this

  override def +(another: Tree): Tree = another

  override def +(value: Double): Tree = Const.by(value)

  override def +(name: String): Tree = new Var(name)

  override def *(another: Tree): Tree = this

  override def *(value: Double): Tree = this

  override def *(name: String): Tree = this
}

object One extends Tree {

  override def eval(args: Arguments): Double = 1

  override def d(targetVariable: String): Tree = Zero

  override def *(another: Tree): Tree = another

  override def *(value: Double): Tree = Const.by(value)

  override def *(name: String): Tree = new Var(name)
}

