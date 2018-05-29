package chapter02

object PolymorphicFunction {
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length <= 1)
      true
    else if (!ordered(as(0), as(1)))
      false
    else
      isSorted(as.tail, ordered)
  }
}
