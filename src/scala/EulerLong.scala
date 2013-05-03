class EulerLong(val n: Long) {
  import EulerLong._

  def isPrime: Boolean = (n > 1) && (primes.takeWhile {_ < scala.math.sqrt(n)}.forall {n % _ != 0})
}

object EulerLong {
  import scala.language.implicitConversions

  implicit def longToEulerLong(value: Long) = new EulerLong(value)

  implicit def intToEulerLong(value: Int) = new EulerLong(value)

  private def fromLong(start: Long, step: Int): Stream[Long] = start #:: fromLong(start + step, step)

  val primes: Stream[Long] = 2 #:: fromLong(3, 2).filter {_.isPrime}
}
