package se.hardchee.S99

object Challenge10 extends Challenge {
  def encode[A](iter: Iterable[A]): Iterable[Tuple2[Int, A]] = {
    Challenge09.pack(iter) map {
      case xs: Iterable[_] => (xs.toList.length, xs.head: A)
    }
  }

  def go = {
    Some(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
