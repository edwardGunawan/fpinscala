package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }
  // it is not tail recursive
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f)) // because you need to TRAVERSE until the end to know to call the function from the end value
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("Tail is empty")
    case Cons(head, tail) => tail
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, tail) => Cons(h, tail)
    case Nil => sys.error("set Head is empty")
  }

  def drop[A](l: List[A], n: Int): List[A] = (n, l) match {
    case (0, lst) => lst
    case (n, Cons(_, tail)) => drop(tail, n-1)
    case _ => Nil
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(head, tail) if f(head) => dropWhile(tail,f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(h,tail) => Cons(h,init(tail))
    case Cons(_, Nil) => Nil
  }
  /*
    Compute the length of the list using foldRight
   */
  def length[A](l: List[A]): Int = foldRight(l,0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Cons(h, tail) => foldLeft(tail, f(z, h))(f)
    case Nil => z
  }

  /*
    Write sum,product and a function to compute the length of a list using foldLeft
   */
  def sumWithFoldLeft(ls:List[Int]):Int = foldLeft(ls, 0)(_ + _)
  def productWithFoldLeft(ls:List[Int]):Int = foldLeft(ls, 1)(_ * _)
  def lengthWithFoldLeft[A](ls:List[A]):Int = foldLeft(ls,0)((acc, _) => acc +1)

  /*
    Write a function taht returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)). See if
    you can write it using fold
   */
  def reverse[A](ls:List[A]): List[A] = foldLeft(ls,List[A]())((acc,head) => Cons(head,acc))

  /*
    Implement foldLeft in terms of foldRight? How about the other way around? Implementing foldRight
    via foldLeft is useful beacuse it lets use implement foldRight
    Great Explanation: https://stackoverflow.com/questions/17136794/foldleft-using-foldright-in-scala
    What happened is that it does function composition from right associativity to left
    Compose is the return on the right side will be the input on the left side
    When you are doing foldLeft, it is the same as doing it foldRight beause it will do right associative.
    so from f(1,(2,f(z, 3))) to f(f(f(z,1),2),3)

    For each element a of l we take the partial application b => f(b,a), which is a function B => B. Then, we compose
    all these functions in such a way that the function corresponding to the rightmost element (with which we start the
    traversal) is at far left in the composition chain. Finally, we apply the big composed function on z.
   */
  def foldLeftFromFoldRight[A,B](as:List[A], s:B)(f:(A,B) => B):B = {
    def h(a: A, g:B => B): (B => B) = g compose ((x:B) => f(a,x)) // equal to (x:B) => g(f(a, x))
    foldRight(as, (b:B) => b)(h)(s)
  }

  def foldRightFromFoldLeft[A,B](as:List[A], s:B)(f:(B, A) => B): B = {
    def h(g: B => B, a:A):(B => B) = (b:B) => g(f(b,a))
    foldLeft(as,(b:B) => b)(h)(s)
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = ???
}
