package se.hardchee.S99

object Challenge18 extends Challenge {
  def slice[A](i: Int, k: Int, input: List[A], out: List[A] = List()): List[A] = input match {
    case Nil => out
    case x :: xs if(i > 0) => slice(i - 1, k - 1, xs, out)
    case x :: xs if(i == 0 && k > 0) => slice(i, k - 1, xs, out :+ x)
    case _ => out
  }

  def go = {
    Some(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}
