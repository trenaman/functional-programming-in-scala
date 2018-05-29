package chapter02

import org.scalatest.{FlatSpec, Matchers}
import chapter02.PolymorphicFunction._

class PolymorphicFunctionSpec extends FlatSpec with Matchers {

  def lessThanOrEqual(a: Int, b: Int) = a <= b

  "isSorted" should "return true for empty array" in {
    val a = Array()
    assertResult(true) {
      isSorted(a, lessThanOrEqual)
    }
  }

  it should "return true for array of size 1" in {
    val a = Array(2)
    assertResult(true) {
      isSorted(a, lessThanOrEqual)
    }
  }

  it should "return true for a sorted array" in {
    val a = Array(1,2,3,4,5)
    assertResult(true) {
      isSorted(a, lessThanOrEqual)
    }
  }

  it should "return false for an unsorted array" in {
    val a = Array(1,2,3,5,4)
    assertResult(false) {
      isSorted(a, lessThanOrEqual)
    }
  }
}
