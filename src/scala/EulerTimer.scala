// Simple class used to keep track of how long a result took to compute.
class EulerTimer {
  var time = System.currentTimeMillis

  // Prints the specified result, the time it took to compute, and resets the timer.
  def printResult(n: Long) {
    println("Result: %d".format(n))
    println("Time  : %.2fs".format((System.currentTimeMillis - time).toFloat / 1000))

    time = System.currentTimeMillis
  }
}

// Convenience methods.
object EulerTimer {
  def apply(value: => Long) {new EulerTimer().printResult(value)}
}
