// Used to represent and manipulate arbitrarily long numbers
// n is the little endian list of the number's digit.
class LongNumber(val n: List[Int]) {
  import LongNumber._

  // Adds this number to the specified one.
  def +(m: LongNumber) = new LongNumber(add(n, m.n))

  // Multiplies this number by the specified one.
  def *(m: LongNumber) = new LongNumber(times(n, m.n))

  // Raises this number to the specified power.
  def pow(n: Int, prod: LongNumber = LongNumber(1)): LongNumber = 
    if(n == 0) prod
    else       pow(n - 1, this * prod)



  // - Helpers ---------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Keeps the first 'count' digits.
  def truncate(count: Int) =  {
    require(count > 0)
    new LongNumber(n.slice(n.length - count, n.length))
  }

  // Pretty printing.
  override def toString() = n.reverse.mkString("")
}


object LongNumber {
  // - Implicit conversions --------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  import scala.language.implicitConversions

  // String to LongNumber
  implicit def apply(value: String) = new LongNumber(value.toList.reverse.map {_ - 48})

  // Long to LongNumber
  implicit def apply(value: Long) = {
    def applyR(n: Long, result: List[Int] = Nil): List[Int] =
        if(n == 0) result
        else       applyR(n / 10, (n % 10).toInt :: result)

    new LongNumber(applyR(value))
  }



  // - Multiplication --------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Multiplies the content of the specified lists.
  private def times(a: List[Int], b: List[Int]) = {
    def timesSimple(a: List[Int], b: Int, c: Int, result: List[Int]): List[Int] = a match {
      case Nil if c == 0 =>  result.reverse
      case Nil           => (c :: result).reverse
      case head :: tail  => {
        val d = head * b + c

        if(d >= 10) timesSimple(tail, b, d / 10, (d % 10) :: result)
        else        timesSimple(tail, b, 0, d :: result)
      }
    }

    def timesR(a: List[Int], b: List[Int], result: List[Int], depth: Int): List[Int] = b match {
      case Nil          => result
      case head :: tail => timesR(a, tail, add(timesSimple(a, head, 0, List.fill(depth)(0)), result), depth + 1)
    }

    timesR(a, b, Nil, 0)
  }



  // - Addition --------------------------------------------------------------------------------------------------------
  // -------------------------------------------------------------------------------------------------------------------
  // Adds the content of the specified lists.
  private def add(a: List[Int], b: List[Int]) = {
    // Sums a.head, b.head and c, returns the result and remaining parts of a and b.
    def sum(a: List[Int], b: List[Int], c: Int) = (a, b) match {
      case (Nil, bh :: bt)      => (bh + c, Nil, bt)
      case (ah :: at, Nil)      => (ah + c, at, Nil)
      case (ah :: at, bh :: bt) => (ah + bh + c, at, bt)

      // This shouldn't happend. If it does, there's a bug somewhere.
      case _                    => throw new IllegalStateException
    }

    // a:      first operand's internal list.
    // b:      second operand's internal list.
    // c:      carry over.
    // result: result's internal list (reversed).
    def addR(a: List[Int], b: List[Int], c: Int, result: List[Int]): List[Int] = (a, b) match {
      // End cases: both lists are empty. We need to reverse the result, as digits are added to the head of the list.
      case (Nil, Nil) if (c != 0) => (c :: result).reverse
      case (Nil, Nil)             => result.reverse

      // Normal case.
      case _ => {
        val (d, at, bt) = sum(a, b, c)

        if(d >= 10) addR(at, bt, 1, (d % 10) :: result)
        else        addR(at, bt, 0, d :: result)
      }
    }

    addR(a, b, 0, Nil)
  }
}
