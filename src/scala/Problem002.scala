/**
  * Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the
  * first 10 terms will be:
  * 
  * 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
  * 
  * By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the
  * even-valued terms.
  */

// Alright, so I might have made this slightly more complicated than it really needs to be.
def collectFibonnacci(max: Int)(accept: (Int => Boolean)) = {
  def collectR(n2: Int, n1: Int, result: List[Int]): List[Int] = {
    val n = n1 + n2

    if(n > max) result
    else if(accept(n)) collectR(n1, n, n :: result)
    else collectR(n1, n, result)
  }

  collectR(1, 2, 2 :: Nil)
}

EulerTimer {collectFibonnacci(4000000) {_ % 2 == 0}.sum}
