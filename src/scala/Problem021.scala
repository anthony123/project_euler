/*
 * Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
 * If d(a) = b and d(b) = a, where a  b, then a and b are an amicable pair and each of a and b are called amicable numbers.
 * 
 * For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
 * The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
 * 
 * Evaluate the sum of all the amicable numbers under 10000.
 */

// This is not much better than brute force.
// - divisors are computed straightforwardly. The only optimisation is that there's no point going over sqrt(n):
//   if m is smaller than sqrt(n), and n is divisible by m, then (n / m) is both greater than sqrt(n) and a divisor of
//   n.
// - amiable numbers are also all evaluated, with the simple optimisation that we only ever evaluate d(d(n)) if
//   d(n) is smaller than n - otherwise we've already evaluated it before and we're wasting cycles.

// Returns the sum of n's proper divisors.
def d(n: Int) = {
  (2 to math.sqrt(n).toInt).foldLeft(1) {(total, m) =>
    if(n % m == 0) {
      val r = n / m
      if(r == m) total + r
      else total + r + m
    }
    else total
  }
}

// Solves the problem for the specified inclusive upper boundary.
def solve(n: Int, result: Int = 0): Int =
  if(n == 1) result

  else {
    val dn = d(n)

    if(dn >= n) solve(n - 1, result)
    else if(d(dn) == n) solve(n - 1, result + dn + n)
    else solve(n - 1, result)
  }


EulerTimer {
  solve(9999)
}

