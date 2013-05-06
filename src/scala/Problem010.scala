/*
 * The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
 * 
 * Find the sum of all the primes below two million.
 */
import EulerLong._

def sumPrimes(n: Int) = primes.takeWhile(_ < n).sum

assert(sumPrimes(10) == 17)

EulerTimer {sumPrimes(2000000)}
