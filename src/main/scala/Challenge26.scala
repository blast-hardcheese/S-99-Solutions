package se.hardchee.S99

object Challenge26 extends Challenge {
  def combinations[A](i: Int, input: List[A]): Iterable[Iterable[A]] = {
    (for(a <- input;
         b <- input;
         c <- input;
         if(a != b && b != c)
         ) yield {
      Set(a, b, c)
    }).toSet
  }

  def go = {
    Some(combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))
  }
}
