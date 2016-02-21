package sctree

import org.scalatest._
import sctree.Tree._

/**
  * Created by Роман on 21.02.2016.
  */
class TreeTest extends FlatSpec with Matchers {

  "A Sum" should "correctly evaluate const and var" in {
    val expression = Tree.===(4) + "x"
    expression.eval({ case "x" => 5 }) should be(9)
  }

  "A Sum" should "correctly evaluate const and const" in {
    val expression = Tree.===(4) + 5
    expression.eval(withNoArgs) should be(9)
  }

  "A Sum" should "correctly evaluate var and var" in {
    val expression = Tree.===("x") + "x"
    expression.eval({ case "x" => 5 }) should be(10)
  }

  "A Sum" should "correctly evaluate var1 and var2" in {
    val expression = Tree.===("x") + "y"
    expression.eval({ case "x" => 5 case "y" => 4 }) should be(9)
  }

  "A Sum" should "have correctly derivative var1 and var2" in {
    val expression = Tree.===("x") + "y"
    expression.d("x") should be(One)
  }

  "A Sum" should "have correctly derivative const and var2" in {
    val expression = Tree.===(4) + "x"
    expression.d("x") should be(One)
  }

  "A Sum" should "have correctly derivative const and const" in {
    val expression = Tree.===(4) + 5
    expression.d("x") should be(Zero)
  }

  "A Mul" should "have correctly derivative var1 and var2" in {
    val expression = Tree.===("x") * "y"
    expression.d("x") should be(Var("y"))
  }

  "A Mul" should "have correctly derivative const and var2" in {
    val expression = Tree.===(4) * "x"
    expression.d("x") should be(Const(4))
  }

  "A Mul" should "have correctly derivative const and const" in {
    val expression = Tree.===(4) * 5
    expression.d("x") should be(Zero)
  }
}
