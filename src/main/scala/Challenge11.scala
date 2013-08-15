package se.hardchee.S99

object Challenge11 extends Challenge {
  def encodeModified[A](iter: Iterable[A]): Iterable[Either[Tuple2[Int, A],A]] = {
    Challenge10.encode(iter).map({
      case (1, x) => Right(x)
      case x => Left(x)
    })
  }

  def go = {
    Some(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
