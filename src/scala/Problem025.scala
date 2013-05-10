/*
 * The Fibonacci sequence is defined by the recurrence relation:
 * 
 * Fn = Fn1 + Fn2, where F1 = 1 and F2 = 1.
 * Hence the first 12 terms will be:
 * 
 * F1 = 1
 * F2 = 1
 * F3 = 2
 * F4 = 3
 * F5 = 5
 * F6 = 8
 * F7 = 13
 * F8 = 21
 * F9 = 34
 * F10 = 55
 * F11 = 89
 * F12 = 144
 * The 12th term, F12, is the first term to contain three digits.
 * 
 * What is the first term in the Fibonacci sequence to contain 1000 digits?
 */

// The only difficulty here is to work with arbitrary long numbers, and we already have a convenient class for that.

def countFibonacci(limit: Int) = {
  def countFibonacciR(n2: LongNumber, n1: LongNumber, count: Int): Int = {
    val n = n2 + n1

    if(n.n.length >= limit) count
    else countFibonacciR(n1, n, count + 1)
  }

  countFibonacciR(1, 1, 3)
}

EulerTimer {countFibonacci(1000)}
