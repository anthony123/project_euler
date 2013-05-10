/*
 * A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example,
 * the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
 * 
 * A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this
 * sum exceeds n.
 * 
 * As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of
 * two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be
 * written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even
 * though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than
 * this limit.
 * 
 * Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
 */

// This is fairly trivial, with two possible traps I fell right into at first:
// - List[Int] really isn't a good data structure for this problem: we must check whether a collection of integers
//   contain a specific one, and sets are *much* faster for this type of operation.
// - This problem should be tackled from the bottom up rather than top down: filling the list of abundant numbers from
//   the biggest to the smallest can't work, as n = m + o implifies that both m and o are smaller than n.

import EulerLong._

def solve(n: Int) = {
  // Computes the increment for the specified int and list of abundant numbers.
  // Either 0 if n can't be written as the sum of two abundant numbers or n otherwise.
  def increment(n: Int, abundants: Set[Int]) =
    if(abundants.exists {m => abundants.contains(n - m)}) 0
    else n

  // Solves the problem from the bottom up.
  def solveR(m: Int, abundants: Set[Int], result: Int): Int = {
    if(m > n) result
    else {
      if(m.divisors.sum > m) solveR(m + 1, abundants + m, result + increment(m, abundants))
      else solveR(m + 1, abundants, result + increment(m, abundants))
    }
  }

  solveR(1, Set[Int](), 0)
}

EulerTimer {solve(28123)}
