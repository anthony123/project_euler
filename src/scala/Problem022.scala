/*
 * Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first
 * names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply
 * this value by its alphabetical position in the list to obtain a name score.
 * 
 * For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53,
 * is the 938th name in the list. So, COLIN would obtain a score of 938 * 53 = 49714.
 * 
 * What is the total of all the name scores in the file?
 */

// Nothing fancy here, I'm just doing exactly what the problem asks me to do.

def solve(names: List[String]) = {
  def solveR(names: List[String], index: Int, result: Int): Int = names match {
    case Nil          => result
    case head :: tail => {
      solveR(tail, index + 1, result + index * head.foldLeft(0) {_ + _ - 64})
    }
  }

  solveR(names.sorted, 1, 0)
}

// Loads all names. We're doing that outside of the EulerTimer's scope because it has no bearing on the algorithm's
// performance.
val names = scala.io.Source.fromFile("names.txt").mkString.split(',').toList.map {_.trim().filterNot (_ == '"')}

EulerTimer {solve(names)}
