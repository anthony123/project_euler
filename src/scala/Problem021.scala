/*
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
 * If d(a) = b and d(b) = a, where a  b, then a and b are an amicable pair and each of a and b are called amicable numbers.
 * 
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
 * The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 * 
 * Evaluate the sum of all the amicable numbers under 10000.
 */

// This is not much better than brute force: we're evaluating almost all amiable numbers, with the simple optimisation
// that we only ever evaluate d(d(n)) if d(n) is smaller than n - otherwise we've already evaluated it before and we're
// wasting cycles.

import EulerLong._

// Solves the problem for the specified inclusive upper boundary.
def solve(n: Long, result: Long = 0): Long =
  if(n == 1) result

  else {
    val dn = n.divisors.sum

    if(dn >= n) solve(n - 1, result)
    else if(dn.divisors.sum == n) solve(n - 1, result + dn + n)
    else solve(n - 1, result)
  }

EulerTimer {solve(9999)}

