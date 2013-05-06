/*
 * Starting in the top left corner of a 2x2 grid, and only being able to move to the right and down, there are exactly 6
 * routes to the bottom right corner.
 * 
 * How many such routes are there through a 20x20 grid?
 */

// Used to cache all known results.
// The subtlety here lies in the fact that results are symetrical: result(a, b) == result(b, a)
class Cache {
  import collection.mutable._
  val cache = Map((1, 1) -> 2l)

  def get(h: Int, v: Int) = {
    if(h > v) cache.get((h, v))
    else cache.get((v, h))
  }

  def set(h: Int, v: Int, value: Long) {
    if(h > v) cache((h, v)) = value
    else cache((v, h)) = value
  }
}

def solve(h: Int, v: Int, cache: Cache = new Cache()): Long = cache.get(h, v) match {
  // We have a cache entry: use that.
  case Some(value) => value

  // We don't: explore deeper.
  case None        => {

    val result = {
      // Shortcut in case of lines or columns.
      if(h == 1 || v == 1) h + v

      else solve(h - 1, v, cache) + solve(h, v - 1, cache)
    }

    cache.set(h, v, result)
    result
  }
}

EulerTimer {solve(20, 20)}
