/*
 * You are given the following information, but you may prefer to do some research for yourself.
 * 1 Jan 1900 was a Monday.
 * Thirty days has September,
 * April, June and November.
 * All the rest have thirty-one,
 * Saving February alone,
 * Which has twenty-eight, rain or shine.
 * And on leap years, twenty-nine.
 * A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
 * How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
 */

// Leap year: divisible by 4, or century and divisible by 400.
def isLeapYear(year: Int) = year % 4 == 0 && (year % 100 != 0 || year % 400 == 0)

// Returns the number of days in the requested month / year.
def daysForMonth(year: Int, month: Int) =
   // February
  if(month == 2) {
    if(isLeapYear(year)) 29
    else 28
  }

  // 30 days months.
  else if(month == 9 || month == 4 || month == 6 || month == 11) 30

  // 31 days month
  else 31

// Solves the problem.
def solve(year: Int, month: Int, day: Int, result: Int = 0): Int = {
  // We're done.
  if(year > 2000) result

  else {
    val d = daysForMonth(year, month)

    // We've finished the current month.
    if(day > d) solve(year, month + 1, day - d, result)

    // We've finished the current year.
    else if(month > 12) solve(year + 1, 1, day, result)

    // This sunday lands on a first of the month.
    else if(day == 1) solve(year, month, day + 7, result + 1)

    // Not a first of the month.
    else solve(year, month, day + 7, result)
  }
}

// Starts on the first sunday of January 1901.
EulerTimer {solve(1901, 1, 6)}
