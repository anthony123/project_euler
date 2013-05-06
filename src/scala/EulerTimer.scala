// Simple class used to keep track of how long a result took to compute.
class EulerTimer {
  var time = System.currentTimeMillis

  def printResult(s: String) {
    println("Result: %s".format(s))
    println("Time  : %.2fs".format((System.currentTimeMillis - time).toFloat / 1000))

    time = System.currentTimeMillis
  }
}

// Convenience methods.
object EulerTimer {
  def apply[T](value: => T) {new EulerTimer().printResult(value.toString)}
}
