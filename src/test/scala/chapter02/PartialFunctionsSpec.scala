package chapter02

import org.scalatest.{FlatSpec, Matchers}
import PartialFunctions.{curry, uncurry, compose}


class PartialFunctionsSpec extends FlatSpec with Matchers {

  "curry" should " return a function that can be partially applied with one parameter" in {
    def mult(a: Int, b: Int) = a * b
    def doubleInt: Int => Int = curry(mult)(2)

    assertResult(6) {
      doubleInt(3)
    }
  }

  "uncurry" should " reverse the transformation of curry" in {
    def mult(a: Int, b: Int) = a * b

    def uncurried = uncurry(curry(mult))

    assertResult(8) {
      uncurried(2,4)
    }
  }

  "compose" should " return a function that composes two functions" in {
    def doubleInt(x: Int) = 2 * x
    def increment(x: Int) = 1 + x
    def doubleAndIncrement = compose(increment, doubleInt)

    assertResult(1) {
      doubleAndIncrement(0)
    }

    assertResult(3) {
      doubleAndIncrement(1)
    }
  }
}
