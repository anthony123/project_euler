/*
 * By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
 * 
 * What is the 10 001st prime number?
 */

import EulerLong._

def findNthPrime(n: Int) = {
  def findR(n: Int, count: Int): Int = {
    if(n.isPrime) {
      if(count == 1) n
      else findR(n + 1, count - 1)
    }
    else findR(n + 1, count)
  }

  findR(2, n)
}

println(findNthPrime(10001))
