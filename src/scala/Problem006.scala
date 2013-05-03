/*
 * The sum of the squares of the first ten natural numbers is,
 * 12 + 22 + ... + 102 = 385
 * 
 * The square of the sum of the first ten natural numbers is,
 * (1 + 2 + ... + 10)^2 = 552 = 3025
 * 
 * Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is
 * 3025  385 = 2640.
 * 
 * Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
 */
def sumSquare(n: Int) = (0 to n).foldLeft(0) {(total, current) => total + current * current}

def squareSum(n: Int) = {
  val sum = (0 to n).sum
  sum * sum
}

def solveFor(n: Int) = squareSum(n) - sumSquare(n)

println(solveFor(100))
