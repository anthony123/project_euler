/*
 * A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2,
 * 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The
 * lexicographic permutations of 0, 1 and 2 are:
 * 
 * 012   021   102   120   201   210
 * 
 * What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
 */

// The number of permutations of n digits is n!
// We have 10 numbers to permute: there are 9! ways of permutating the last 9.
// 1000000 / 9! = 2: number of complete loops to reach the millionth permutation. Our first digit is then list[2], or 2.
// By recursing on 1000000 % 9!, we'll reach the desired result.
//
// The bit that threw me for a loop was asking for the millionth permutation - I've always been a sucker for off-by-1
// errors.

def count(n: Int, c: Int): Int =
  if(n == 0) c
  else count(n - 1, c * n)

def recurse(n: Int, input: List[Int], output: List[Int]): List[Int] = {
  if(n == 0) (input ::: output).reverse
  else {
    val c = count(input.length - 1, 1)
    val p = n / c
    val i = input.splitAt(p)

    recurse(n % c, i._1 ::: i._2.tail, input(p) :: output)
  }
}
// List(2, 7, 8, 3, 9, 1, 5, 6, 4, 0)
//      2  7  8  3  9  1  5  4  6  0

EulerTimer {recurse(1000000 - 1, List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9), Nil).mkString}
