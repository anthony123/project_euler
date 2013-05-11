/*
 * A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is
 * 9009 = 91 99.
 * 
 * Find the largest palindrome made from the product of two 3-digit numbers.
 */

// Explores all palindromes, with the following optimisations:
// - we only explore about half of the possible products: i * j == j *, there's no point in checking both.
// - by starting from the highest boundaries, we are more likely to find the largest possible palindrome earlier. Since
//   we only check for numbers that might be larger than the current maximum, we drastically reduce the number of
//   values for which we must call isPalindrome.

// Checks whether n is a palindrome.
def isPalindrome(n: Int) = {
  def isPalindromeR(n: Int, m: Int): Int = 
    if(n == 0) m
    else isPalindromeR(n / 10, m * 10 + n % 10)

  isPalindromeR(n, 0) == n
}

// Looks for the largest palindrome made from the product of numbers between min & max.
def findMax(min: Int, max: Int) = {
  def findMaxR(i: Int, j: Int, current: Int): Int = {
    // i is below the minimum value, we're done.
    if(i < min) current

    // j is smaller than i, we don't want to explore this: if i * j is a palindrome, so is j * i. We need only analyse
    // one of them.
    else if(j < i) findMaxR(i - 1, max, current)

    else {
      val p = i * j

      // Only checks whether p is a palindrome if there's a chance it might be of interest.
      if(p > current && isPalindrome(p)) findMaxR(i, j - 1, p)
      else findMaxR(i, j - 1, current)
    }
  }

  findMaxR(max, max, 0)
}

// TODO: this can be improved by starting from 999 down to 100 and ignoring numbers whose product is already known
// to be smaller than our max.
assert(isPalindrome(9009))

EulerTimer {findMax(100, 999)}
