/*
 * If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19
 * letters used in total.
 * 
 * If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
 * 
 * NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115
 * (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with
 * British usage.
 */
import collection.mutable._

object Cache {
  // Stores the number of characters that must be added for each 'depth'.
  val depthLengths = Array(0, 0, "hundredand".length, "thousand".length)

  // Numbers for which we need a hard-coded value as there's little logic behind their spelling.
  val numberLengths = Map(
    10 -> "ten".length,
    1  -> "one".length,
    11 -> "eleven".length,
    2  -> "two".length,
    12 -> "twelve".length,
    20 -> "twenty".length,
    3  -> "three".length,
    13 -> "thirteen".length,
    30 -> "thirty".length,
    4  -> "four".length,
    14 -> "fourteen".length,
    40 -> "forty".length,
    5  -> "five".length,
    15 -> "fifteen".length,
    50 -> "fifty".length,
    6  -> "six".length,
    16 -> "sixteen".length,
    60 -> "sixty".length,
    7  -> "seven".length,
    17 -> "seventeen".length,
    70 -> "seventy".length,
    8  -> "eight".length,
    18 -> "eighteen".length,
    80 -> "eighty".length,
    9  -> "nine".length,
    19 -> "nineteen".length,
    90 -> "ninety".length)

  // Numbers for which having a pre-computed value greatly simplifies the algorithm.
  for(i <- 21 to 99 if i % 10 != 0) numberLengths(i) = {
    val m = i % 10
    numberLengths(i - m) + numberLengths(m)
  }

  // Numbers for which having a pre-computed value allows me to be lazy.
  for(i <- 1 to 9) numberLengths(i * 100) = numberLengths(i) + "hundred".length

  def store(n: Int, l: Int) = {
    numberLengths(n) = l
    l
  }

  def get(n: Int) = numberLengths.get(n)
}

def solve(n: Int): Int = {
  // Counts the number of digits in the specified int.
  def length(n: Int, result: Int = 0): Int =
    if(n < 10) result
    else length(n / 10, result + 1)

  def solveR(n: Int, result: Int): Int = {
    // We've found our solution.
    if(n == 0) result

    // Look into the cache for known value.
    else Cache.get(n) match {
      case Some(value) => result + value
      case None        => {
        val l = length(n)
        val p = math.pow(10, l)

        result + Cache.store(n, solveR((n % p).toInt, result + Cache.depthLengths(l) + Cache.numberLengths((n / p).toInt)))
      }
    }
  }

  solveR(n, 0)
}

EulerTimer {(1 to 1000).foldLeft(0) {_ + solve(_)}}
