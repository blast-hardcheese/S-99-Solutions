package se.hardchee.S99

object Challenge09 extends Challenge {
  def pack[A](iter: Iterable[A]): Iterable[Iterable[A]] = {
    iter.foldLeft[List[List[A]]](List())({
      case (sofar@Nil, next) => sofar :+ List(next)
      case (sofar, next) if(sofar.last.last == next) => sofar.init :+ (sofar.last :+ next)
      case (sofar, next) => sofar :+ List(next)
    })
  }

  def go = {
    Some(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
  }
}
