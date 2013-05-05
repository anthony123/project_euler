/*
 * The following iterative sequence is defined for the set of positive integers:
 * 
 * n -> n/2 (n is even)
 * n -> 3n + 1 (n is odd)
 * 
 * Using the rule above and starting with 13, we generate the following sequence:
 * 
 * 13 -> 40 -> 20 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
 * 
 * It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been
 * proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
 * 
 * Which starting number, under one million, produces the longest chain?
 * 
 * NOTE: Once the chain starts the terms are allowed to go above one million.
 */
import collection.mutable._

// Computes the length of the chain starting with the specified value, using cache to store intermediate results.
def lengthFor(n: Long, cache: Map[Long, Int]): Int = {
  // We already know the value for n, return it.
  if(cache.contains(n)) cache(n)

  // We don't: request it for the next value and cache it.
  else {
    cache(n) = lengthFor(if(n % 2 == 0) n / 2 else 3 * n + 1, cache) + 1
    cache(n)
  }
}

val cache = Map(1l -> 1)
(1 to 1000000).foldLeft(0) {(max, n) => math.max(max, lengthFor(n, cache))}

// At this point, cache contains all possible values, look for the maximum one.
println(cache.maxBy(_._2)._1)

