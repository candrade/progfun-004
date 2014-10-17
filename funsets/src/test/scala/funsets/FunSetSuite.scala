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
  test("string take") {
    val message = "hello, world"
    assert(message.take(5) == "hello")
  }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  test("adding ints") {
    assert(1 + 2 === 3)
  }

  
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
    val multiplesOfTwo = (x: Int) => x % 2 == 0
    val multiplesOfFive = (x: Int) => x % 5 == 0
    val isEven = (x: Int) => x % 2 == 0
    val wholeNumbers = (x: Int) => x > 0
    val setOfEvens = filter(wholeNumbers, isEven)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   * 
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  ignore("singletonSet(1) contains 1") {
    
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
    }
  }

  ignore("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains inner elements") {
    new TestSets {
      val intersection =
        intersect(multiplesOfTwo, multiplesOfFive)

      assert(contains(intersection, 10), "Intersection 1")
      assert(contains(intersection, 20), "Intersection 2")
      assert(!contains(intersection, 9), "Intersection 3")
    }
  }

  test("diff contains left elements") {
    new TestSets {
      val difference =
        diff(multiplesOfFive, multiplesOfTwo)

      assert(contains(difference, 5), "Diff 1")
      assert(contains(difference, 25), "Diff 2")
      assert(!contains(difference, 10), "Diff 3")
    }
  }

  test("filter contains elements that pass predicate") {
    new TestSets {
      assert(contains(setOfEvens, 2), "Filter 1")
      assert(contains(setOfEvens, 4), "Filter 2")
      assert(!contains(setOfEvens, 1), "Filter 3")
      assert(!contains(setOfEvens, 3), "Filter 3")
    }
  }

  test("forall tests if all elements pass predicate") {
    new TestSets {
      val testOdds = (x: Int) => x > 0 && x % 2 == 1

      // Tests that all numbers have to be odd
      assert(forall(testOdds, (x: Int) => x % 2 == 1), "ForAll 1")
      // Tests that all numbers are less than five (which is false)
      assert(!forall(testOdds, (x: Int) => x < 5), "ForAll 2")
      // Tests that all numbers are even (which is false for odds)
      assert(!forall(testOdds, (x: Int) => x % 2 == 0), "ForAll 3")
    }
  }

  test("exists tests if at least 1 element passes predicate") {
    new TestSets {
      val divisibleByFive = (x: Int) => x % 5 == 0
      val lessThanZero = (x: Int) => x < 0

      assert(exists(multiplesOfTwo, divisibleByFive), "Exists 1")
      assert(exists(setOfEvens, divisibleByFive), "Exists 2")
      assert(!exists(setOfEvens, lessThanZero), "Exists 3")
    }
  }

  test("map transforms set into another by defined func") {
    new TestSets {
      val setOfOdds = map(setOfEvens, (x: Int) => x + 1)

      assert(contains(setOfOdds, 3), "Map 1")
      assert(!contains(setOfOdds, 6), "Map 2")
    }
  }
}
