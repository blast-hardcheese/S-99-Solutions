package se.hardchee.S99

object Challenge37 extends Challenge {
  // Add solution here

  def go = {
    import arithmetic.S99Int._
    Some("%s %s".format(10.totient, 10.improvedTotient))
    Some(10.inefficientTotient == 10.improvedTotient)
  }
}
