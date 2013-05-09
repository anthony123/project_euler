/**
  * Long utility class. In its current state, this is mostly used to factorize prime number manipulation.
  */
class EulerLong(val n: Long) {
  import EulerLong._

  // Checks whether the current instance is a prime number.
  def isPrime: Boolean = (n > 1) && (primes.takeWhile {_ <= scala.math.sqrt(n)}.forall {n % _ != 0})

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

  // Stream.from implementation that returns a Stream[Long] rather than Stream[Int] (some of Project Euler's problems
  // require us to work with numbers that don't fit in an Int).
  private def fromLong(start: Long, step: Int): Stream[Long] = start #:: fromLong(start + step, step)

  // Stream of all prime numbers. I probably wouldn't do that in production code as it's a memory leak waiting to
  // happen, but it's nice for simple scripts.
  val primes: Stream[Long] = 2 #:: fromLong(3, 2).filter {_.isPrime}
}
