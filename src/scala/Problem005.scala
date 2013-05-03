/*
 * 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
 * 
 * What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
 */

def isCandidate(n: Int, by: Int): Boolean = 
  if(by == 1) true
  else if(n % by != 0) false
  else isCandidate(n, by - 1)

def findFirstCandidate(max: Int) = {
  def findR(n: Int, max: Int): Int = 
    if(isCandidate(n, max)) n
    else findR(n + 1, max)

  findR(1, max)
}

println(findFirstCandidate(20))
