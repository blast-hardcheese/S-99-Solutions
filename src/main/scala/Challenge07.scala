package se.hardchee.S99

object Challenge07 extends Challenge {
  def flatten(structure: Iterable[Any]): List[Any] = {
    structure.foldLeft[List[Any]](List[Any]())({
      case (last, next: Iterable[_]) => last ::: flatten(next)
      case (last, next) => last :+ next
    })
  }

  def go = {
    Some(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
  }
}
