package se.hardchee.S99

object Challenge08 extends Challenge {
  def compress[A](iter: Iterable[A]): Iterable[A] = {
    iter.foldLeft[List[A]](List())({
      case (sofar@Nil, next) => sofar :+ next
      case (sofar, next) if(sofar.last != next) => sofar :+ next
      case (sofar, _) => sofar
    })
  }

  def go = {
    Some(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
