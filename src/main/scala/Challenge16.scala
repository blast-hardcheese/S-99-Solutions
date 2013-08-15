package se.hardchee.S99

object Challenge16 extends Challenge {
  def drop[A](n: Int, iter: Iterable[A], sofar: List[A] = List(), counter: Int = 0): Iterable[A] = (iter: Iterable[_]) match {
    case list if(list.isEmpty) => sofar
    case list if(counter == n) => drop(n, iter.tail, sofar, 0)
    case _ => drop(n, iter.tail, sofar :+ iter.head, counter + 1)
  }

  def go = {
    Some(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}
