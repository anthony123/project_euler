/*
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 * 
 * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 */

// The mathematical solution here is sound, but the Scala implementation might need some work.
// Instead of analysing all possible products, this code finds each number's prime factors and keeps the minimum
// possible. For example:
// - 3 => (3)
// - 9 => (3, 3)
// This means we need to keep at least (3, 3) in our list of factors.
// Once the minimum set of prime factors has been identified, multiply them and you have your result.
//
// In the case of 10, we have (2, 2, 2, 3, 3, 5, 7) = 2^3 * 3^2 * 5 * 7 = 2520


import EulerLong._

def solve(n: Long) = {
  // This relies on the fact that EulerLong returns sorted prime factors.
  def merge(what: List[Long], into: Map[Long, Long]): Map[Long, Long] = what match {
    case head :: tail => {
      val (h, t) = tail.span {_ == head}
      val c = h.length + 1
      if(!into.contains(head) || into(head) < c) merge(t, into + (head -> c))
      else merge(t, into)
    }
    case Nil          => into
  }

  def solveR(n: Long, result: Map[Long, Long] = Map[Long, Long]()): Map[Long, Long] = {
    if(n == 1) result
    else solveR(n - 1, merge(n.primeFactors, result))
  }

  solveR(n).foldLeft(1l) {(a, b) => a * math.pow(b._1, b._2).toLong}
}

assert(solve(10) == 2520)

EulerTimer {solve(20)}
