package se.hardchee.S99

object Challenge17 extends Challenge {
  def split[A](n: Int, tail: List[A], head: List[A] = List()): Tuple2[List[A],List[A]] = tail match {
    case _ if(n == 0) => (head, tail)
    case x :: xs => split(n - 1, xs, head :+ x)
    case xs@Nil => (head, xs)
  }

  def go = {
    Some(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}
