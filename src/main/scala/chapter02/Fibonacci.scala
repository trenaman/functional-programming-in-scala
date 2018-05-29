package chapter02

object Fibonacci {

  def fib(n: Int): Int = {
    require(n >= 0, "Fibonacci is undefined for negative numbers.")

    def fibTailRec(i: Int, fib_i_minus_2: Int, fib_i_minus_1: Int): Int = {
      if (i == n)
        fib_i_minus_2 + fib_i_minus_1
      else
        fibTailRec(i + 1, fib_i_minus_1, fib_i_minus_2 + fib_i_minus_1)
    }

    n match {
      case 0 => 0
      case 1 => 1
      case _ => fibTailRec(2, 0, 1)
    }
  }

}
