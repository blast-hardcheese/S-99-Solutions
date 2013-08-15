package se.hardchee.S99

object Challenge21 extends Challenge {
  def insertAt[A](elem: A, offset: Int, input: List[A], head: List[A] = List()): List[A] = input match {
    case xs if(offset == 0) => head ::: elem :: xs
    case x :: xs if(offset > 0) => insertAt(elem, offset - 1, xs, head :+ x)
    case xs => xs
  }

  def go = {
    Some(insertAt('new, 1, List('a, 'b, 'c, 'd)))
  }
}
