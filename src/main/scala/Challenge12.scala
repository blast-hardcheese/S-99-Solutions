package se.hardchee.S99

object Challenge12 extends Challenge {
  def decode[A](iter: Iterable[Tuple2[Int, A]]): Iterable[A] = {
    for(pair <- iter; i <- 0 until pair._1) yield pair._2
  }

  def go = {
    Some(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
  }
}
