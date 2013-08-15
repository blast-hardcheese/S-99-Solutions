package se.hardchee.S99

object Challenge15 extends Challenge {
  def duplicateN[A](n: Int, iter: Iterable[A]): Iterable[A] = {
    for(elem <- iter; i <- 0 until n) yield elem
  }

  def go = {
    Some(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
  }
}
