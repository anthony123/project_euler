/*
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143 ?
 */
def findMaxPrimeFactor(n: Long) = {
  import EulerLong._

  def findR(n: Long, primes: Stream[Long], result: List[Long]): List[Long] = {
    // n isn't divisible anymore: we're done.
    if(n.isPrime) n :: result

    // n is still divisible, find its remaining prime factors.
    else {
      val m = primes.head

      // We're not using primes.tail in the recursion because nothing says that prime factors can't be duplicated.
      if(n % m == 0) findR(n / m, primes, m :: result)
      else findR(n, primes.tail, result)
    }
  }

  findR(n, primes, Nil).max
}

println(findMaxPrimeFactor(600851475143l))

