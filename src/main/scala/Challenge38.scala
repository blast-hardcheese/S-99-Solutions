package se.hardchee.S99

object Challenge38 extends Challenge {
  def go = {
    import arithmetic.S99Int._

    Some(10090.inefficientTotient == 10090.improvedTotient)
  }
}
