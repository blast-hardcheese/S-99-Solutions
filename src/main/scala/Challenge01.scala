package se.hardchee.S99

object Challenge01 extends Challenge {
  def last(l: List[Int]): Option[Any] = l match {
    case x :: Nil => Some(x)
    case x :: xs => last(xs)
    case Nil => None
  }

  def go = {
    last(List(1, 1, 2, 3, 5, 8))
  }
}
