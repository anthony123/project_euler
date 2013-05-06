/*
 * A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is
 * 9009 = 91 99.
 * 
 * Find the largest palindrome made from the product of two 3-digit numbers.
 */

// Checks whether n is a palindrome.
def isPalindrome(n: Int) = {
  def isPalindromeR(n: Int, m: Int): Int = 
    if(n == 0) m
    else isPalindromeR(n / 10, m * 10 + n % 10)

  isPalindromeR(n, 0) == n
}

// We keep j < i to make sure we don't check for the same pair more than once (2 * 3 and 3 * 2, for example).
EulerTimer {
  val palindromes = for(i <- 100 to 999; j <- 100 to i; k = i * j; if isPalindrome(k)) yield k
  palindromes.max
}
