/*
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these
 * multiples is 23.
 * 
 * Find the sum of all the multiples of 3 or 5 below 1000.
 */

def sumDivisibleBy(what: Int, by: Int) = {
  val p = (what - 1) / by

  by * (p * (p + 1)) / 2
}

EulerTimer {sumDivisibleBy(1000, 3) + sumDivisibleBy(1000, 5) - sumDivisibleBy(1000, 15)}

