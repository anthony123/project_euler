/*
 * By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from
 * top to bottom is 23.
 * 
 * 3
 * 7 4
 * 2 4 6
 * 8 5 9 3
 * 
 * That is, 3 + 7 + 4 + 9 = 23.
 * 
 * Find the maximum total from top to bottom of the triangle below:
 * 
 * 75
 * 95 64
 * 17 47 82
 * 18 35 87 10
 * 20 04 82 47 65
 * 19 01 23 75 03 34
 * 88 02 77 73 07 63 67
 * 99 65 04 28 06 16 70 92
 * 41 41 26 56 83 40 80 70 33
 * 41 48 72 33 47 32 37 16 94 29
 * 53 71 44 65 25 43 91 52 97 51 14
 * 70 11 33 28 77 73 17 78 39 68 17 57
 * 91 71 52 38 17 14 91 43 58 50 27 29 48
 * 63 66 04 68 89 53 67 30 73 16 69 87 40 31
 * 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23
 */

// From the bottom up:
// - consider each 2 by 2 triangle, from the left to the right.
// - turn each triangle in a single value: the sum of its head and the maximum value of its base
//   For example:
//   4
//   1 2
//   => 4 + max(1, 2) = 6
// - remove the last line of the triangle
//
// Iterate this until the triangle is empty, the result is single remaining value.
def solve(data: Array[Array[Int]]) = {
  import math.max

  def solveR(data: Array[Array[Int]], history: Array[Int]): Int = {
    if(data.length == 0) history(0)
    else {
      val head = data.head

      for(i <- 0 until head.length) history(i) = head(i) + max(history(i), history(i + 1))

      solveR(data.tail, history)
    }
  }

  solveR(data.tail, data.head)
}

def parse(definition: String) = raw.split('\n').map(_.split(' ').map {_.toInt}).reverse

// For problem 67, replace this by scala.io.Source.fromFile("/path/to/triangle.txt").mkString
val raw = """75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"""

EulerTimer {solve(parse(raw))}

