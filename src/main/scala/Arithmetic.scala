package arithmetic {
  class S99Int(val start: Int) {
    import S99Int._

    def isNotPrime = (2 to start / 2).foldLeft(false)( (last, next) => last || (start % next == 0) )
    def isPrime = ! isNotPrime

    def isCoprimeTo(other: Int) = gcd(start, other) == 1

    def pow(n: Int, r: Int = 1): Int = if(n <= 0) r else pow(n - 1, r * start)
    def inefficientTotient = (1 to start).flatMap({ n => if(isCoprimeTo(n)) Some(n) else None }).length
    def improvedTotient = primeFactorMultiplicity.foldLeft(1)({ case (last, (p, m)) => last * (p-1)*p.pow(m-1) })
    def totient = improvedTotient

    def allFactors = {
      @scala.annotation.tailrec
      def getAllFactors(i: Int, until: Int, allFactors: List[Int] = List()): List[Int] = {
        if(i == until) {
          allFactors
        } else {
          val isNotPrime = (for(j <- allFactors) yield i % j == 0) contains true
          val isPrime = ! isNotPrime
          val newPrimes = if(isPrime) allFactors :+ i else allFactors
          getAllFactors(i + 1, until, newPrimes)
        }
      }

      getAllFactors(2, start)
    }


    def primeFactors = {
      def getPrimeFactors(n: Int, factors: List[Int] = List()): List[Int] = {
        val nextFactor = smallestFactor(n)
        val nextN = n / nextFactor
        val newFactors = factors :+ nextFactor
        if(nextFactor == n) newFactors
        else getPrimeFactors(nextN, newFactors)
      }

      getPrimeFactors(start)
    }

    def primeFactorMultiplicity = primeFactors.foldLeft[List[(Int, Int)]](List())( (last, next) => last match {
      case x :: xs if(x._1 == next) => (x._1, x._2 + 1) :: xs
      case xs => (next, 1) :: xs
    }).reverse
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    def gcd(l: Int, r: Int): Int = (l, r) match {
      case (l, r) if(l < 0 || r < 0) => 1
      case (l, r) if(l > r) => gcd(l - r, r)
      case (l, r) if(l < r) => gcd(l, r - l)
      case (l, r) if(l == r) => l
    }

    @annotation.tailrec
    def smallestFactor(limit: Int, n: Int = 2): Int = {
      if(limit % n == 0) n
      else smallestFactor(limit, n + 1)
    }

    def listPrimesinRange(r: Range): List[Int] = {
      def collectPrimes(i: Int, primes: List[Int], seed: List[Int]): List[Int] = {
        val isPrime = (seed ::: primes).foldLeft(true)( (last, next) => last && i % next != 0 )
        if(isPrime) primes :+ i
        else primes
      }
      def rangePrimes(range: Range, seed: List[Int] = List()) =
        range.foldLeft[List[Int]](List())( (last, next) => collectPrimes(next, last, seed) )
      val startPrimes = rangePrimes(2 until r.start)
      rangePrimes(r, startPrimes)
    }
  }
}
