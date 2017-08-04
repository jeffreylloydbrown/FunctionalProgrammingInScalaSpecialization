package funsets


/**
  * 2. Purely Functional Sets.
  */
object FunSets {
  /**
    * We represent a set by its characteristic function, i.e.
    * its `contains` predicate.
    */
  type Set = Int => Boolean

  /**
    * Indicates whether a set contains a given element.
    */
  def contains (s: Set, elem: Int): Boolean = s(elem)

  /**
    * Returns the set of the one given element.  The implementation is to
    * return a function that takes and argument and compares it to elem.
    */
  def singletonSet (elem: Int): Set = (x: Int) => (x == elem)

  /**
    * An empty or null set.  All possible elements map to false,
    * so no ints are in it.   Therefore, empty.
    */
  val nullSet: Set = (x: Int) => false

  /**
    * Returns the set of all integers between and including 2 endpoints.
    *
    * @param start The first integer in the range
    * @param end   The last integer in the range
    * @return The function expression representing the set.
    */
  def rangeSet (start: Int, end: Int): Set = (x: Int) => (start <= x && x <= end)

  /**
    * Returns the union of the two given sets,
    * the sets of all elements that are in either `s` or `t`.
    */
  def union (s: Set, t: Set): Set = (x: Int) => (s(x) || t(x))

  /**
    * Returns the intersection of the two given sets,
    * the set of all elements that are both in `s` and `t`.
    */
  def intersect (s: Set, t: Set): Set = (x: Int) => (s(x) && t(x))

  /**
    * Returns the difference of the two given sets,
    * the set of all elements of `s` that are not in `t`.
    */
  def diff (s: Set, t: Set): Set = (x: Int) => (s(x) && !t(x))

  /**
    * Returns the subset of `s` for which `p` holds.
    */
  def filter (s: Set, p: Int => Boolean): Set = (x: Int) => (s(x) && p(x))

  /**
    * The bounds for `forall` and `exists` are +/- 1000.
    */
  val bound = 1000

  /**
    * Returns whether all bounded integers within `s` satisfy `p`.
    */
  def forall (s: Set, p: Int => Boolean): Boolean = {
    def iter (a: Int): Boolean = {
      if (contains(s, a) && !p(a)) false
      else if (a > bound) true        // we made it all the way thru to +bound with p() true.
      else iter(a + 1)
    }
    iter(-bound)
  }

  /**
    * Returns whether there exists a bounded integer within `s`
    * that satisfies `p`.
    *
    * We are to implement this with forall(), above.  forall() runs
    * until it finds a failure then stops with false.  exists() needs to run
    * until it finds a success then stop with true.  Suggests an implementation,
    * doesn't it.
    */
  def exists (s: Set, p: Int => Boolean): Boolean = !forall(s, (x: Int) => !p(x))

  /**
    * Returns a set transformed by applying `f` to each element of `s`.
    */
  def map (s: Set, f: Int => Int): Set = {
    def iter (a: Int, transform: Set): Set = {
      if (a > bound) transform        // we're done, return the new set
      else if (contains(s, a)) iter(a+1, union(transform, singletonSet(f(a))))
      else iter(a + 1, transform)
    }

    iter(-bound, nullSet)
  }

  /**
    * Displays the contents of a set
    */
  def toString (s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  /**
    * Prints the contents of a set on the console.
    */
  def printSet (label: String, s: Set) {
    println(label + ": " + toString(s))
  }
}
