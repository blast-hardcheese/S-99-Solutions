package se.hardchee.S99

object Challenge39 extends Challenge {
  def go = {
    import arithmetic.S99Int._
    Some(listPrimesinRange(7 to 31))
  }
}
