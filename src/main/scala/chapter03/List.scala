package chapter03

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty)
      Nil
    else
      Cons(as.head, apply(as.tail: _*))
  }

  def tail[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](h: A, l: List[A]): List[A] = Cons(h, l)

  def drop[A](l: List[A], n: Int): List[A] = {
    def loop[A](iter: Int, as: List[A]): List[A] = {
      if (iter == n) {
        as
      } else {
        loop(iter + 1, tail(as))
      }

    }

    loop(0, l)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else l
    }
  }
}