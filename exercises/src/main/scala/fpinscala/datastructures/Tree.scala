package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree:Tree[A]):Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r) + 1
  }
  /*
    Making a List of tree and keep destructuring like dfs so you are able to call with one argument call and tail recursive
   */
  def sizeTailRecur[A](tree:Tree[A]): Int = {
    @tailrec
    def inner(l:List[Tree[A]], acc:Int): Int = l match {
      case Nil => acc
      case Cons(Leaf(v),ls) => inner(ls, acc+1)
      case Cons(Branch(a,b),ls) => inner(Cons(a,Cons(b,ls)), acc +1 )
    }

    inner(List(tree), 0)
  }

  def max(tree:Tree[Int]):Int = tree match {
    case Leaf(v) => v
    case Branch(l,r) => max(l) max max(r)
  }

  def depth[A](tree:Tree[A]): Int = tree match {
    case Leaf(v) => 0
    case Branch(l,r) => (depth(l) max depth(r)) + 1
  }

  def map[A,B](tree:Tree[A])(f:A => B):Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l,r) => Branch(map(l)(f), map(r)(f))
  }
  /*
    Just like FoldRight for List fold receive a "handler" for each data constructor of a type, and recursively
    accumulate some values using these handlers.
   */
  def fold[A,B](a:Tree[A])(f: A => B)(g: (B,B) => B): B = a match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def foldViaSize[A](a:Tree[A]): Int = fold(a)(a => 1)(_ + _ + 1)

  def maxViaFold(a:Tree[Int]):Int = fold(a)(identity)(_ max _)

  def foldViaDepth[A](tree:Tree[A]):Int = fold(tree)(_ => 0)((d1,d2) => (d1 max d2) + 1)

  def mapViaFold[A,B](tree:Tree[A])(f:A => B):Tree[B] = fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_,_))



}