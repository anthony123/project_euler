/*
 * A Pythagorean triplet is a set of three natural numbers, a  b  c, for which,
 * a2^ + b^2 = c^2
 * 
 * For example, 32 + 42 = 9 + 16 = 25 = 52.
 * 
 * There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 * Find the product abc.
 */

def solve(a: Int = 1, b: Int = 1): Int = {
  val c = 1000 - a - b
  if(a * a + b * b == c * c) a * b * c
  else if(b < 999) solve(a, b + 1)
  else if(a < 999) solve(a + 1, 1)
  else throw new NoSuchElementException
}

EulerTimer {solve()}
