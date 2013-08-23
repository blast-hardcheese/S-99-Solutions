package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

    def isNotPrime = (2 to start / 2).foldLeft[Boolean](false)( (last, next) => last || (start % next == 0) )
    def isPrime = ! isNotPrime
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def gcd(l: Int, r: Int): Int = (l, r) match {
      case (l, r) if(l < 0 || r < 0) => 1
      case (l, r) if(l > r) => gcd(l - r, r)
      case (l, r) if(l < r) => gcd(l, r - l)
      case (l, r) if(l == r) => l
    }
  }
}
