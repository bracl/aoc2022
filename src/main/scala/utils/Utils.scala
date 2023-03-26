package utils

object Utils {

  implicit class TakeUntilListWrapper[T](list: List[T]) {
    def takeUntil(predicate: T => Boolean): List[T] = {
      list.span(predicate) match {
        case (head, tail) => head ::: tail.take(1)
      }
    }
  }

}
