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
    * val s1 = singletonSet(1)
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

    val u12 = union(s1, s2)
    val u23 = union(s2, s3)

    def s: Int => Set = item => singletonSet(item)
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      assert(contains(u12, 1), "Union 1")
      assert(contains(u12, 2), "Union 2")
      assert(!contains(u12, 3), "Union 3")
    }
  }

  test("intersect contains elements which exists in each set") {
    new TestSets {
      val i12 = intersect(s1, s2)
      assert(!contains(i12, 2), "intersect s1 + s2: 2")
      assert(!contains(i12, 1), "intersect s1 + s2: 1")

      val i33 = intersect(s3, s3)
      assert(contains(i33, 3), "intersect s3 + s3: 3")

      val iu12u23 = intersect(u12, u23)
      assert(contains(iu12u23, 2), "intersect s1 + s2 and s2 + s3: 2")
      assert(!contains(iu12u23, 1), "intersect s1 + s2 and s2 + s3: 1")
      assert(!contains(iu12u23, 3), "intersect s1 + s2 and s2 + s3: 3")
    }
  }

  test("diff contains all elements in first set that are not in second") {
    new TestSets {
      val du12u23 = diff(u12, u23)
      assert(contains(du12u23, 1), "diff s1 + s2 and s2 + s3: 1")
      assert(!contains(du12u23, 2), "diff s1 + s2 and s2 + s3: 2")
      assert(!contains(du12u23, 3), "diff s1 + s2 and s2 + s3: 3")
    }
  }

  test("filter selects elements of a set that are accepted by given predicate") {
    new TestSets {
      def p: Int => Boolean = item => item < 2

      val fu12 = filter(u12, p)
      assert(contains(fu12, 1), "filter s1 + s2 and > 1: 1")
      assert(!contains(fu12, 2), "filter s1 + s2 and > 1: 2")
    }
  }

  test("forall tests condition for all items in the set") {
    new TestSets {
      def p: Int => Boolean = item => item > 0

      val positives = forall(u12, p)
      assert(positives === true, "all items in u12 are positive")

      val usm1u12 = union(singletonSet(-1), u12)
      assert(contains(usm1u12, -1), "-1 is in usm1u12")

      var withNegative = forall(usm1u12, p)
      assert(withNegative === false, "not all items in usm1u12 are positive")
    }
  }

  test("exists tests that condition is true for at least on item in the set") {
    new TestSets {
      def p: Int => Boolean = item => item == 1

      val oneExists = exists(u12, p)
      assert(oneExists === true, "one exists in u12")

      val oneDoesntExist = exists(u23, p)
      assert(oneDoesntExist === false, "one doesn't exist in u23")
    }
  }

  test("map transforms given set to another set") {
    new TestSets {
      def p: Int => Boolean = item => item < 0

      def invert: Int => Int = item => -item

      val invu12 = map(u12, invert)
      assert(contains(invu12, -1), "invu12 contains -1")
      assert(contains(invu12, -2), "invu12 contains -2")
      assert(!contains(invu12, 1), "invu12 doesn't contain 1")
      assert(!contains(invu12, 2), "invu12 doesn't contain 2")
    }
  }

  ignore("minus one") {
    new TestSets {
      val set = union(union(s(1), s(3)), union(union(s(4), s(5)), union(s(7), s(1000))))

      def minus1: Int => Int = item => item - 1

      printSet(set)
      printSet(map(set, minus1))
    }
  }
}
