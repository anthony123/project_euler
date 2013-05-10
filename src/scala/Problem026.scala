/*
 * A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to
 * 10 are given:
 * 
 * 1/2	= 	0.5
 * 1/3	= 	0.(3)
 * 1/4	= 	0.25
 * 1/5	= 	0.2
 * 1/6	= 	0.1(6)
 * 1/7	= 	0.(142857)
 * 1/8	= 	0.125
 * 1/9	= 	0.(1)
 * 1/10	= 	0.1
 * Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring
 * cycle.
 * 
 * Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
 */

// I'm frankly a bit hazy on why this works:
// - Testing against a remainder of 1 works for all prime numbers I've tested.
// - There does not seem to be any need to test anything but prime numbers - I instinctively feel that if n is not
//   a prime, 1 / n is not going to have a longer chain than any of n's prime factors. Couldn't prove it to save my
//   life, though.

def length(n: Int) = {
  def lengthR(rem: Int, count: Int): Int = {
    if(rem == 0) 0
    else if(rem == 1) count
    else lengthR(rem * 10 % n, count + 1)
  }
  lengthR(10 % n, 1)
}

EulerTimer {
  EulerLong.primes.takeWhile {_ < 1000}.foldLeft((0, 0)) {(x, y) =>
    val l = length(y.toInt)
    if(x._1 < l) (l, y.toInt)
    else x
  }._2
}
