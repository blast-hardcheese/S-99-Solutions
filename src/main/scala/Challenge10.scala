package se.hardchee.S99

object Challenge10 extends Challenge {
  def encode[A](iter: Iterable[A]): Iterable[Tuple2[Int, A]] = {
    iter.foldLeft[List[Tuple2[Int, A]]](List())({
      case (sofar, next) if(!sofar.isEmpty && sofar.last._2 == next) => sofar.init :+ (sofar.last._1 + 1, next)
      case (sofar, next) => sofar :+ (1, next)
    })
  }

  def go = {
    Some(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
