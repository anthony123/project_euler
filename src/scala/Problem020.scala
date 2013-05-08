/*
 * n! means n x (n - 1) x ... x 3 x 2 x 1
 * 
 * For example, 10! = 10 x 9 x ... x 3 x 2 x 1 = 3628800,
 * and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
 * 
 * Find the sum of the digits in the number 100!
 */

// Since we already have a class for multiplying arbitrarily long numbers together, this is trivial.
// I'm quite happy with how easy to read implicit conversions make that code.
def fact(n: Int, result: LongNumber = 1): LongNumber = {
  if(n == 0) result
  else       fact(n - 1, result * n)
}

EulerTimer {fact(100).n.sum}
