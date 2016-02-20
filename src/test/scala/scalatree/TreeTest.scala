package scalatree

import org.scalatest.{Matchers, FlatSpec}
import scalatree.Tree._

/**
  * Created by rprokhorov on 2/20/16.
  */
class TreeTest extends FlatSpec with Matchers {

  "A Const" should "correctly evaluate const" in {
    val expression = new Const(5)
    expression.eval(withNoArgs) should be(5)
  }
}