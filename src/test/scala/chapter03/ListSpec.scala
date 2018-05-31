package chapter03

import org.scalatest.{FlatSpec, Matchers}
import List._

class ListSpec extends FlatSpec with Matchers {

  "Pattern matching" should "return first matching case" in {
    // The exercise asks what is the result of the following case statement;
    // I believe the answer is 3.
    //
    assertResult(3) {
      List(1,2,3,4,5) match {
        case Cons(x, Cons(2, Cons(4, _))) => x
        case Nil => 42
        case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
        case Cons(h, t) => h + sum(t)
        case _ => 101
      }
    }
  }

  "tail" should "return Nil for empty list" in {
    assertResult(Nil) {
      tail(Nil)
    }
  }

  it should "return Nil for list of length 1" in {
    assertResult(Nil) {
      tail(Cons(1, Nil))
    }
  }

  it should "return the tail for longer lists" in {
    val t = Cons(2, Cons(3, Nil))
    val l = Cons(1, t)
    assertResult(t) {
      tail(l)
    }
  }

  "setHead" should "return a single element list if applied on Nil" in {
    assertResult(Cons(1, Nil)) {
      setHead(1, Nil)
    }
  }

  it should "insert the new element at the head of the list" in {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    assertResult(Cons(42, l)) {
      setHead(42, l)
    }
  }

  "drop" should "return Nil if the list is empty" in {
    assertResult(Nil) {
      drop(Nil, 0)
    }
    assertResult(Nil) {
      drop(Nil, 1)
    }
  }

  it should "return Nil if you drop with n = l.size" in {
    assertResult(Nil) {
      drop(Cons(1, Cons(2, Cons(3, Nil))), 3)
    }
  }

  it should "return the remainder of the list if the list if n < l.size" in {
    val l = Cons(1, Cons(2, Cons(3, Nil)))
    assertResult(Cons(3, Nil)) {
      drop(l, 2)
    }
  }

  "dropWhile" should "return Nil when list is empty" in {
    assertResult(Nil) {
      dropWhile(Nil, (_: Int) => true)
    }
  }

  it should "remove elements from head while they match the predicate" in {
    val l = Cons(1, Cons(2, (Cons(3, Cons(2, Cons(1, Nil))))))
    assertResult(Cons(3, Cons(2, Cons(1, Nil)))) {
      dropWhile(l, (x: Int) => x < 3)
    }
  }

  it should "leave list unchanged if first element does not match predicate" in {
    val l = Cons(1, Cons(2, Nil))
    assertResult(l) {
      dropWhile(l, (x: Int) => x < 0)
    }
  }
}
