package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
    val sRange0to20 = rangeSet(0, 20)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
      assert(!contains(s1, 4), "s1 doesn't contain 4")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)

      printSet("union s", s)
      
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("rangeSet creates an inclusive integer range") {
    new TestSets {
      val s = rangeSet(4,6)
      printSet("rangeSet", s)
      assert(!contains(s, 3), "rangeSet(4,6) has no 3")
      assert(contains(s, 4), "rangeSet(4,6) has 4")
      assert(contains(s, 5), "rangeSet(4,6) has 5")
      assert(contains(s, 6), "rangeSet(4,6) has 6")
      assert(!contains(s, 7), "rangeSet(4,6) has no 7")
    }
  }

  test("intersection tests") {
    new TestSets {
      val s31 = union(s3, s1)
      val s13 = union(s1, s3)

      printSet("intersection s31", s31)
      printSet("intersection s13", s13)

      val just2 = intersect(s2, sRange0to20)
      val none = intersect(s1, rangeSet(-4,0))
      val just3 = intersect(s3, intersect(s31, s13))

      printSet("intersection just2", just2)
      printSet("intersection none", none)
      printSet("intersection just3", just3)

      assert(!contains(just2, 1), "just2 has no 1")
      assert(contains(just2, 2), "just2 has 2")
      assert(!contains(just2, 3), "just2 has no 3")

      assert(!contains(none, 1), "none has no 1")
      assert(!contains(none, 2), "none has no 2")
      assert(!contains(none, 3), "none has no 3")

      assert(!contains(just3, 1), "just3 has no 1")
      assert(!contains(just3, 2), "just3 has no 2")
      assert(contains(just3, 3), "just3 has 3")
    }
  }

  test("diff tests") {
    new TestSets {
      val range0to5 = rangeSet(0,5)
      val range2to3 = rangeSet(2,3)

      val no2no3 = diff(range0to5, range2to3)
      val none = diff(range2to3, range0to5)

      printSet("diff range0to5", range0to5)
      printSet("diff range2to3", range2to3)
      printSet("diff no2no3", no2no3)
      printSet("diff none", none)

      assert(!contains(no2no3, -1), "no2no3 has no -1")
      assert(contains(no2no3, 0), "no2no3 has 0")
      assert(contains(no2no3, 1), "no2no3 has 1")
      assert(!contains(no2no3, 2), "no2no3 has no 2")
      assert(!contains(no2no3, 3), "no2no3 has no 3")
      assert(contains(no2no3, 4), "no2no3 has 4")
      assert(contains(no2no3, 5), "no2no3 has 5")
      assert(!contains(no2no3, 6), "no2no3 has no 6")

      assert(!contains(none, -1), "none has no -1")
      assert(!contains(none, 0), "none has no 0")
      assert(!contains(none, 1), "none has no 1")
      assert(!contains(none, 2), "none has no 2")
      assert(!contains(none, 3), "none has no 3")
      assert(!contains(none, 4), "none has no 4")
      assert(!contains(none, 5), "none has no 5")
      assert(!contains(none, 6), "none has no 6")
    }
  }

  test("filter tests") {
    new TestSets {
      def isEven (x: Int): Boolean = ((x%2) == 0)

      val evens = filter(rangeSet(10,16), isEven)

      printSet("evens", evens)

      assert(!contains(evens,  8), "evens doesn't have 8")
      assert(!contains(evens,  9), "evens doesn't have 9")
      assert( contains(evens, 10), "evens has 10")
      assert(!contains(evens, 11), "evens doesn't have 11")
      assert( contains(evens, 12), "evens has 12")
      assert(!contains(evens, 13), "evens doesn't have 13")
      assert( contains(evens, 14), "evens has 14")
      assert(!contains(evens, 15), "evens doesn't have 15")
      assert( contains(evens, 16), "evens has 16")
      assert(!contains(evens, 17), "evens doesn't have 17")
      assert(!contains(evens, 18), "evens doesn't have 18")
    }
  }

  test("forall tests") {
    new TestSets {
      printSet("forall sRange0to20", sRange0to20)

      assert( forall(sRange0to20, (x: Int) => (x < 30)), "forall sRange0to20 < 30")
      assert(!forall(sRange0to20, (x: Int) => (x < 10)), "forall sRange0to20 not < 10")
    }
  }

  test("exists tests") {
    new TestSets {
      printSet("exists sRange0to20", sRange0to20)

      assert( exists(sRange0to20, (x: Int) => (x == 12)), "exists sRange0to20 = 12")
      assert(!exists(sRange0to20, (x: Int) => (x == 21)), "exists sRange0to20 not = 21")
    }
  }

  test("map tests") {
    new TestSets {
      val original = rangeSet(0,3)
      val cubed = map(original, (x: Int) => x*x*x)

      printSet("original", original)
      printSet("cubed", cubed)

      assert( contains(cubed, 8), "cubed has 8")
      assert(!contains(cubed, 9), "cubed doesn't have 9")
    }
  }

}
