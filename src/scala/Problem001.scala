/*
 * If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these
 * multiples is 23.
 * 
 * Find the sum of all the multiples of 3 or 5 below 1000.
 */

// A match is defined here as an int that's divisible by 3 or 5.
def sumMatches(range: Range) = range.filter(x => x % 3 == 0 || x % 5 == 0).foldLeft(0) {_ + _}

assert(sumMatches(0 until 10) == 23)

EulerTimer {sumMatches(0 until 1000)}
