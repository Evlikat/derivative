package exercises

import org.scalatest._
import Exercises._

/**
  * Created by rprokhorov on 2/24/16.
  */
class ExercisesTest extends FlatSpec with Matchers {

  "An add" should "correctly sum two positive values" in {
    val actual = add(100, 35)
    actual should be(135)
  }

  "An add" should "correctly sum two negative values" in {
    val actual = add(-100, -35)
    actual should be(-135)
  }

  "An add" should "correctly sum positive and negative values" in {
    val actual = add(-100, 35)
    actual should be(-65)
  }

  "A sum" should "correctly sum values" in {
    val actual = sum((1 to 100) toList)
    actual should be(5050)
  }

  "A length" should "correctly return length of a list" in {
    val actual = Exercises.length((1 to 100) toList)
    actual should be(100)
  }

  "A map" should "correctly return map a list" in {
    val actual = map[Int, Int](List(1, 2, 3), x => x + 1)
    actual should be((2 to 4) toList)
  }

  "A filter" should "correctly return filter a list" in {
    val actual = filter[Int](List(1, 2, 3, 4), x => x % 2 == 0)
    actual should be(List(2, 4))
  }

  "An append" should "correctly append two lists" in {
    val actual = append[Int](List(1, 2, 3, 4), List(5, 6, 7, 8))
    actual should be((1 to 8) toList)
  }

  "A concat" should "correctly append multiple lists" in {
    val actual = concat[Int](List(List(1, 2), List(3, 4), List(5, 6), List(7, 8)))
    actual should be((1 to 8) toList)
  }

  "A concatMap" should "correctly append multiple lists" in {
    val actual = concatMap[Int, Int](List(1, 2, 3), x => List(x, -1 * x))
    actual should be(List(1, -1, 2, -2, 3, -3))
  }

  "A maximum" should "correctly calculate maximum element" in {
    val actual = maximum(List(1, 2, 13, 4, 5, 6, 7, 8))
    actual should be(Some(13))
  }

  "A reverse" should "correctly reverse a list" in {
    val actual = reverse[Int](List(1, 2, 3))
    actual should be(List(3, 2, 1))
  }
}
