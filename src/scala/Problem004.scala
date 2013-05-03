/*
 * A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is
 * 9009 = 91 99.
 * 
 * Find the largest palindrome made from the product of two 3-digit numbers.
 */

def isPalindrome(n: Int) = {
  val m = n.toString()

  m == m.reverse
}

val palindromes = for(i <- 0 to 999; j <- 0 to 999; k = i * j; if isPalindrome(k)) yield k

println(palindromes.max)
