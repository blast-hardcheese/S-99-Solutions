package se.hardchee.S99

object Challenge02 extends Challenge {
  def penultimate(l: List[Int]): Option[Any] = l match {
    case x1 :: x2 :: Nil => Some(x1)
    case x :: xs => penultimate(xs)
    case Nil => None
  }

  def go = {
    penultimate(List(1, 1, 2, 3, 5, 8))
  }
}
