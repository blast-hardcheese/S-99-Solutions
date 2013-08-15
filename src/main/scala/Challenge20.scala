package se.hardchee.S99

object Challenge20 extends Challenge {
  def removeAt[A](i: Int, input: List[A], head: List[A] = List()): Tuple2[List[A],Option[A]] = input match {
    case x :: xs if(i > 0) => removeAt(i - 1, xs, head :+ x)
    case x :: xs if(i == 0) => (head ::: xs, Some(x))
    case _ => (head ::: input, None)
  }

  def go = {
    Some(removeAt(1, List('a, 'b, 'c, 'd)))
  }
}
