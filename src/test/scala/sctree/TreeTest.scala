package sctree

import org.scalatest._
import sctree.Tree._

/**
  * Created by Роман on 21.02.2016.
  */
class TreeTest extends FlatSpec with Matchers {

  "A Sum" should "correctly evaluate const and var" in {
    val expression = exp ? 4 + "x"
    expression.eval({ case "x" => 5 }) should be(9)
  }

  "A Sum" should "correctly evaluate const and const" in {
    val expression = exp ? 4 + 5;
    expression.eval() should be(9)
  }

  "A Sum" should "correctly evaluate var and var" in {
    val expression = exp ? "x" + "x"
    expression.eval({ case "x" => 5 }) should be(10)
  }

  "A Sum" should "correctly evaluate var1 and var2" in {
    val expression = exp ? "x" + "y"
    expression.eval({ case "x" => 5 case "y" => 4 }) should be(9)
  }

  "A Sum" should "have correct derivative var1 and var2" in {
    val expression = exp ? "x" + "y"
    expression.d("x") should be(One)
  }

  "A Sum" should "have correct derivative const and var2" in {
    val expression = exp ? 4 + "x"
    expression.d("x") should be(One)
  }

  "A Sum" should "have correct derivative const and const" in {
    val expression = exp ? 4 + 5
    expression.d("x") should be(Zero)
  }

  "A Mul" should "have correct derivative var1 and var2" in {
    val expression = exp ? "x" * "y"
    expression.d("x") should be(Var("y"))
  }

  "A Mul" should "have two correct derivatives var1 and var2" in {
    val expression = exp ? "x" * "y"
    expression.d("x").d("y") should be(One)
  }

  "A Mul" should "have correct derivative const and var2" in {
    val expression = exp ? 4 * "x"
    expression.d("x") should be(Const(4))
  }

  "A Mul" should "have correct derivative const and const" in {
    val expression = exp ? 4 * 5
    expression.d("x") should be(Zero)
  }

  "A Mul and Sum" should "have correct derivative" in {
    val expression = Var("x", 2) + (Var("x") * 4) + 3
    expression.d("x") should be(Const(2.0) * Var("x") + Const(4))
  }

  "A Degree Var" should "have correct derivative" in {
    val expression = Var("x", 3)
    expression.d("x") should be(Const(3.0) * Var("x", 2))
  }

  "A Degree Var" should "have correct sum same names and degrees" in {
    val expression = Var("x", 3) + Var("x", 3)
    expression should be(Const(2.0) * Var("x", 3))
  }

  "A Degree Var" should "have correct sum same names and different degrees" in {
    val expression = Var("x", 3) + Var("x", 2)
    expression should be(Var("x", 3) + Var("x", 2))
  }

  "A Degree Var" should "have correct sum different names" in {
    val expression = Var("x", 3) + Var("y", 3)
    expression should be(Var("x", 3) + Var("y", 3))
  }

  "A Degree Var" should "have correct mul" in {
    val expression = Var("x", 3) * Var("x", -3)
    expression should be(One)
  }
}
