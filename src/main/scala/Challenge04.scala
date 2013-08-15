package se.hardchee.S99

object Challenge04 extends Challenge {
  def length[A](list: List[A], curLength: Int = 0): Option[Int] = list match {
    case x :: xs => length(xs, curLength + 1)
    case Nil => Some(curLength)
  }

  def go = {
    length(List(1, 1, 2, 3, 5, 8))
  }
}
