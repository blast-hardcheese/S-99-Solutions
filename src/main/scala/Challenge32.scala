package se.hardchee.S99

object Challenge32 extends Challenge {
  // Add solution here

  def go = {
    import arithmetic.S99Int._

    Some(gcd(36, 63))
  }
}
