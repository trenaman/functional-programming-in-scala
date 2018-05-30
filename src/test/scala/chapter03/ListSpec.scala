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

}
