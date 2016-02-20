package tree

import org.scalatest._
import tree.Tree._
/**
  * Created by rprokhorov on 2/20/16.
  */
class TreeTest extends FlatSpec with Matchers {

  "A Const" should "correctly evaluate const" in {
    val expression = Tree exp + 5
    expression.eval(withNoArgs) should be (5)
  }

  "A Var" should "correctly evaluate var" in {
    val expression = Tree exp "x"
    expression.eval({ case "x" => 5 }) should be (5)
  }

  "A Sum" should "correctly evaluate two consts" in {
    val expression = Tree exp 4 + 5
    expression.eval(withNoArgs) should be (9)
  }

  "A Sum" should "correctly evaluate two vars" in {
    val expression = exp("x") + "x"
    expression.eval({ case "x" => 5 }) should be (10)
  }

  "A Sum" should "correctly evaluate var and const" in {
    val expression = exp("x") + 4
    expression.eval({ case "x" => 5 }) should be (9)
  }

  "A Sum" should "have correct derivative from const and const" in {
    val expression = Tree exp 4 + 5
    (expression d "x").eval(withNoArgs) should be (0)
  }

  "A Sum" should "have correct derivative from var and const" in {
    val expression = exp("x") + 5
    (expression d "x").eval(withNoArgs) should be (1)
  }

  "A Sum" should "have correct derivative from var and var" in {
    val expression = exp("x") + "x"
    (expression d "x").eval(withNoArgs) should be (2)
  }

  "A Sum" should "have correct derivative from different vars" in {
    val expression = exp("x") + "y"
    (expression d "x").eval(withNoArgs) should be (1)
  }

  "A Sum" should "have correct two derivatives from different vars" in {
    val expression = exp("x") + "y"
    (expression d "x").d("y").eval(withNoArgs) should be (0)
  }

  "A Mul" should "correctly multiply two const" in {
    val expression = Tree exp 5 * 4
    expression.eval(withNoArgs) should be (20)
  }

  "A Mul" should "correctly multiply var and const" in {
    val expression = exp(4) * "x"
    expression.eval({ case "x" => 5}) should be (20)
  }

  "A Mul" should "correctly multiply two vars" in {
    val expression = exp("y") * "x"
    expression.eval({ case "x" => 5 case "y" => 4}) should be (20)
  }

  "A Mul" should "have correct derivative from const and const" in {
    val expression = Tree exp 4 * 5
    (expression d "x").eval(withNoArgs) should be (0)
  }

  "A Mul" should "have correct derivative from var and const" in {
    val expression = exp("x") * 5
    (expression d "x").eval(withNoArgs) should be (5)
  }

  "A Mul" should "have correct derivative from var and var" in {
    val expression = exp("x") * "x"
    (expression d "x").eval({ case "x" => 5 }) should be (10)
  }

  "A Mul" should "have correct derivative from different vars" in {
    val expression = exp("x") * "y"
    (expression d "x").eval({ case "y" => 4}) should be (4)
  }

  "A Mul" should "have correct two derivatives from different vars" in {
    val expression = exp("x") * "y"
    (expression d "x").d("y").eval(withNoArgs) should be (1)
  }
}
