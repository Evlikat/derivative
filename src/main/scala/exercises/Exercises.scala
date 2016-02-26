package exercises

import scala.annotation.tailrec

// You are not permitted to use these List methods:
// * length
// * map
// * filter
// * ::: (and variations such as ++)
// * flatten
// * flatMap
// * reverse (and variations i.e. reverseMap, reverse_:::)
// This also means you are not permitted to use for-comprehensions on Lists.
// You are permitted to use the functions you write yourself. For example, Exercise 2 may use Exercise 1 or Exercise 3.
// Using permitted existing methods where appropriate will attract marks for elegance.

// TOTAL marks:    /66

object Exercises {
  def succ(n: Int) = n + 1
  def pred(n: Int) = n - 1

  // Exercise 1
  // Relative Difficulty: 1
  // Correctness: 2.0 marks
  // Performance: 0.5 mark
  // Elegance: 0.5 marks
  // Total: 3
  @tailrec
  def add(x: Int, y: Int): Int = {
    if (Math.abs(x) < Math.abs(y)) add(y, x) else
    if (y > 0) add(succ(x), pred(y)) else if (y < 0) add(pred(x), succ(y)) else x
  }

  // Exercise 2
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def sum(x: List[Int]): Int = {
    @tailrec
    def sum(acc: Int, x: List[Int]): Int = if (x.isEmpty) acc else sum(add(acc, x.head), x.tail)
    if (x.isEmpty) 0 else sum(x.head, x.tail)
  }

  // Exercise 3
  // Relative Difficulty: 2
  // Correctness: 2.5 marks
  // Performance: 1 mark
  // Elegance: 0.5 marks
  // Total: 4
  def length[A](x: List[A]): Int = {
    @tailrec def length[A](acc: Int, x: List[A]): Int = if (x.isEmpty) acc else length(succ(acc), x.tail)
    if (x.isEmpty) 0 else length(1, x.tail)
  }

  // Exercise 4
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.0 mark
  // Elegance: 1.5 marks
  // Total: 7
  def map[A, B](x: List[A], f: A => B): List[B] = {
    @tailrec
    def map[A, B](acc: List[B], x: List[A], f: A => B): List[B] = if (x.isEmpty) acc else map(append(acc, f(x.head)), x.tail, f)
    if (x.isEmpty) Nil else map(List(f(x.head)), x.tail, f)
  }

  // Exercise 5
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def filter[A](x: List[A], f: A => Boolean): List[A] = {
    @tailrec
    def filter[A](acc: List[A], x: List[A], f: A => Boolean): List[A] = {
      if (x.isEmpty) acc else filter(if (f(x.head)) append(acc, x.head) else acc, x.tail, f)
    }
    if (x.isEmpty) Nil else filter(Nil, x, f)
  }

  // Exercise 6
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def append[A](x: List[A], y: List[A]): List[A] = {
    @tailrec
    def app[A](x: List[A], y: List[A]) : List[A] = {
      if (x.isEmpty) y else app(x.tail, x.head :: y)
    }
    app(reverse(x), y)
  }

  // to implement ???
  private def append[A](x: List[A], el: A): List[A] = x :+ el

  // Exercise 7
  // Relative Difficulty: 5
  // Correctness: 4.5 marks
  // Performance: 1.5 marks
  // Elegance: 1 mark
  // Total: 7
  def concat[A](x: List[List[A]]): List[A] = {
    @tailrec
    def concat[A](acc: List[A], x: List[List[A]]): List[A] = {
      if (x.isEmpty) acc
      else concat(append(acc, x.head), x.tail)
    }
    concat(Nil, x)
  }

  // Exercise 8
  // Relative Difficulty: 7
  // Correctness: 5.0 marks
  // Performance: 1.5 marks
  // Elegance: 1.5 mark
  // Total: 8
  def concatMap[A, B](x: List[A], f: A => List[B]): List[B] = {
    @tailrec
    def concatMap[A, B](acc: List[B], x: List[A], f: A => List[B]): List[B] = {
      if (x.isEmpty) acc else concatMap(append(acc, f(x.head)), x.tail, f)
    }
    if (x.isEmpty) Nil else concatMap(Nil, x, f)
  }

  // Exercise 9
  // Relative Difficulty: 8
  // Correctness: 3.5 marks
  // Performance: 3.0 marks
  // Elegance: 2.5 marks
  // Total: 9
  def maximum(x: List[Int]): Option[Int] = {
    @tailrec
    def maximum(currMax: Int, x: List[Int]): Int = if (x.isEmpty) currMax else maximum(Math.max(currMax, x.head), x.tail)
    if (x.isEmpty) None else Option(maximum(x.head, x.tail))
  }

  // Exercise 10
  // Relative Difficulty: 10
  // Correctness: 5.0 marks
  // Performance: 2.5 marks
  // Elegance: 2.5 marks
  // Total: 10
  def reverse[A](x: List[A]): List[A] = {
    @tailrec
    def reverse[A](acc: List[A], x: List[A]): List[A] = if (x.isEmpty) acc else reverse(x.head :: acc, x.tail)
    if (x.isEmpty) Nil else reverse(Nil, x)
  }
}