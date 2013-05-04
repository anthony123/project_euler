/*
 * The sequence of triangle numbers is generated by adding the natural numbers. So the 7th triangle number would be
 * 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. 
 * The first ten terms would be:
 * 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
 * 
 * Let us list the factors of the first seven triangle numbers:
 *  1: 1
 *  3: 1,3
 *  6: 1,2,3,6
 * 10: 1,2,5,10
 * 15: 1,3,5,15
 * 21: 1,3,7,21
 * 28: 1,2,4,7,14,28
 * 
 * We can see that 28 is the first triangle number to have over five divisors.
 * 
 * What is the value of the first triangle number to have over five hundred divisors?
 */

def countDivisors(n: Long) = {
  def countR(m: Double, count: Int): Int = {
    if(m < 0) count
    else if(n % m == 0) countR(m - 1, count + 2)
    else countR(m - 1, count)
  }

  countR(math.floor(math.sqrt(n)), 0)
}

def triangles(n: Long = 0, m: Long = 1)(eval: (Long) => Boolean): Long = {
  val o = n + m

  if(eval(o)) o
  else triangles(o, m + 1)(eval)
}

println(triangles() {countDivisors(_) >= 500})

