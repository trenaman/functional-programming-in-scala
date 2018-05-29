package chapter02

import org.scalatest._
import Fibonacci._

class FibonacciSpec extends FlatSpec with Matchers {

  "fib" should "return correct values for first 10 Fibonacci numbers" in {
    val firstTenFibs = Seq(0,1,1,2,3,5,8,13,21,34)

    firstTenFibs.zipWithIndex.map { case (fibN, n) =>
      assertResult(fibN) {
        fib(n)
      }
    }
  }

  it should "throw IllegalArgumentException for n < 0" in {
    assertThrows[IllegalArgumentException] {
      fib(-1)
    }
  }
}
