package se.hardchee.S99

object Challenge25 extends Challenge {
  def randomPermute[A](input: List[A]): List[A] = {
    Challenge23.randomSelect(input.length, input)
  }

  def go = {
    Some(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
  }
}
