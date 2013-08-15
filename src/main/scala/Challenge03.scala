package se.hardchee.S99

object Challenge03 extends Challenge {
  def nth(k: Int, l: List[Any]): Option[Any] = l match {
    case x :: xs if(k == 0) => Some(x)
    case x :: xs => nth(k - 1, xs)
    case Nil => None
  }

  def go = {
    nth(2, List(1, 1, 2, 3, 5, 8))
  }
}
