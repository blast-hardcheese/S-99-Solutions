package se.hardchee.S99

object Challenge19 extends Challenge {
  def rotate[A](i: Int, input: List[A]): List[A] = input match {
    case x :: xs if(i > 0) => rotate(i - 1, xs :+ x)
    case xs if(i < 0) => rotate(i + 1, xs.last +: xs.init)
    case xs => xs
  }

  def go = {
    Some((
      rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)),
      rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
    ))
  }
}
