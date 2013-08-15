package se.hardchee.S99

object Challenge22 extends Challenge {
  def range(start: Int, end: Int, sofar: List[Int] = List()): List[Int] = {
    if(start == end) {
      sofar
    } else if(start > end) {
      range(start - 1, end, sofar :+ start)
    } else if(start <= end) {
      range(start + 1, end, sofar :+ start)
    } else {
      sofar
    }
  }

  def go = {
    Some((
      range(4, 9),
      range(9, 4)
    ))
  }
}
