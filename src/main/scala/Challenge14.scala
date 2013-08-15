package se.hardchee.S99

object Challenge14 extends Challenge {
  def duplicate[A](iter: Iterable[A]): Iterable[A] = {
    for(elem <- iter; i <- 0 until 2) yield elem
  }

  def go = {
    Some(duplicate(List('a, 'b, 'c, 'c, 'd)))
  }
}
