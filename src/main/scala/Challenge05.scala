package se.hardchee.S99

object Challenge05 extends Challenge {
  def reverse[A](list: List[A]): List[A] = list match {
    case x :: xs => reverse(xs) :+ x
    case Nil => Nil
  }

  def go = {
    Some(reverse(List(1, 1, 2, 3, 5, 8)))
  }
}
