/*
 * Euler published the remarkable quadratic formula:
 * 
 * n² + n + 41
 * 
 * It turns out that the formula will produce 40 primes for the consecutive values n = 0 to 39. However, when
 * n = 40, 402 + 40 + 41 = 40(40 + 1) + 41 is divisible by 41, and certainly when n = 41, 41² + 41 + 41 is clearly
 * divisible by 41.
 * 
 * Using computers, the incredible formula  n² + 79n + 1601 was discovered, which produces 80 primes for the consecutive
 * values n = 0 to 79. The product of the coefficients, 79 and 1601, is 126479.
 * 
 * Considering quadratics of the form:
 * 
 * n² + an + b, where |a| < 1000 and |b| < 1000
 * 
 * where |n| is the modulus/absolute value of n
 * e.g. |11| = 11 and |4| = 4
 * Find the product of the coefficients, a and b, for the quadratic expression that produces the maximum number of
 * primes for consecutive values of n, starting with n = 0.
 */

// Straightforward implementation.
// Note that this could in theory be optimized by excluding all non-primes for possible values of b:
// if n == 0, n^2 + an + b = b.
// In practice though, this is more work than it's worth: checking whether a given value of b is a prime is very similar
// (in terms of cost) as checking whether n^2 + an + b is a prime for n = 0.
// I consider here that the potential performance gain isn't worth the sacrifice in code legibility.

import EulerLong._

def length(a: Int, b: Int, current: Int = 0): Int = {
  if((current * (current + a) + b).isPrime) length(a, b, current + 1)
  else current
}

def solve(a: Int, b: Int, max: Int = 0, prod: Int = 0): Int = {
  // We've explored all values of a.
  if(a <= -1000) prod

  // We've explored all values of b for the current a.
  else if(b <= -1000) solve(a - 1, 999, max, prod)

  else {
    val l = length(a, b)
    if(l > max) solve(a, b - 1, l, a * b)
    else solve(a, b - 1, max, prod)
  }
}

EulerTimer {solve(999, 999)}
