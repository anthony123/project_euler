class EulerLong(val n: Long) {
  import EulerLong._

  def isPrime: Boolean = (n > 1) && (primes.takeWhile {_ < scala.math.sqrt(n)}.forall {n % _ != 0})
}

object EulerLong {
  import scala.language.implicitConversions

  implicit def longToEulerLong(value: Long) = new EulerLong(value)

  implicit def intToEulerLong(value: Int) = new EulerLong(value)

  val primes = 2 #:: Stream.from(3, 2).filter {_.isPrime}
}
