/*
 * The prime factors of 13195 are 5, 7, 13 and 29.
 * 
 * What is the largest prime factor of the number 600851475143 ?
 */
import EulerLong._

def findPrimeFactors(n: Long) = {
  def findPrimeFactorsR(n: Long, m: Long, result: List[Long]): List[Long] =  {
    if(n.isPrime) n :: result
    else if(m.isPrime && n % m == 0) findPrimeFactorsR(n / m, m, m :: result)
    else findPrimeFactorsR(n, m + 1, result)
  }

  findPrimeFactorsR(n, 2, Nil).max
}

println(findPrimeFactors(600851475143l))
