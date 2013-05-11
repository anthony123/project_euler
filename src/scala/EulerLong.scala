/**
  * Utility class used to factorize recurrent number manipulation problems.
  * Project Euler is particularly keen on prime numbers and prime factors, and this class handles them as fast as I
  * know how. It's not terribly memory efficient, however.
  */
class EulerLong(val n: Long) {
  import EulerLong._

  // Checks whether the current instance is a prime number.
  def isPrime: Boolean = checkPrime(n)

  // Returns the current instance's prime factors.
  def primeFactors() = divisors.filter {checkPrime}

  // Returns the current instance's divisors.
  // The "clever" bit here is that there is no need to check for numbers larger than sqrt(n).
  // If n = m * o:
  // - m = o,  which means that sqrt(n) is a divisor.
  // - m != o, in which case one is larger and the other smaller than sqrt(n). By checking until sqrt(n), we will
  //           find all values of m such that n = m * o. For each m, we've also found o = n / m.
  def divisors =  {
    def divisorsR(m: Long, result: List[Long]): List[Long] = {
      if(m == 1) result
      else if(n % m == 0) {
        val r = n / m
        if(r == m) divisorsR(m - 1, r :: result)
        else divisorsR(m - 1, m :: r :: result)
      }
      else divisorsR(m - 1, result)
    }

    divisorsR(math.sqrt(n).toLong, List(1))
  }
}

object EulerLong {
  // Implicit conversions from number types to EulerLong.
  import scala.language.implicitConversions
  implicit def longToEulerLong(value: Long) = new EulerLong(value)
  implicit def intToEulerLong(value: Int) = new EulerLong(value)

  // Checks whether the specified long is a prime number.
  private def checkPrime(n: Long): Boolean = (n > 1) && (primes.takeWhile {_ <= scala.math.sqrt(n)}.forall {n % _ != 0})

  // Stream.from implementation that returns a Stream[Long] rather than Stream[Int] (some of Project Euler's problems
  // require us to work with numbers that don't fit in an Int).
  private def fromLong(start: Long, step: Int): Stream[Long] = start #:: fromLong(start + step, step)

  // Stream of all prime numbers. I probably wouldn't do that in production code as it's a memory leak waiting to
  // happen, but it's nice for simple scripts.
  // Note that we increment the counter by 2 because 2 is the only even prime number and it's already in the stream,
  // so we can skip all other even numbers.
  val primes: Stream[Long] = 2 #:: fromLong(3, 2).filter {_.isPrime}
}
