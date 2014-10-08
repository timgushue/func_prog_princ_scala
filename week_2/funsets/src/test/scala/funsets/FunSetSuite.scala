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

  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

    val evenNumbers: Set = x => x % 2 == 0
    val twoAndFive: Set = x => x == 2 || x == 5
  }

  test("singletonSet(1) contains 1") {
    new TestSets {
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains only shared elements") {
    new TestSets {
      val s = intersect(s1, s2)
      val i = intersect(evenNumbers, twoAndFive)
      assert(!contains(s, 1), "Intersect does not contain first singleton set")
      assert(!contains(s, 2), "Intersect does not contain second singleton set")
      assert(contains(i, 2), "Intersect predicates contains 2")
      assert(!contains(i, 5), "Intersect predicates does not contain 5")
    }
  }

  test("diff does not contains shared elements") {
    new TestSets {
      val s = diff(s1, s2)
      val i = diff(evenNumbers, twoAndFive)
      assert(contains(s, 1), "Diff singletons does contain elements from first set")
      assert(!contains(s, 2), "Diff singletons does not contain element from second set")
      assert(!contains(i, 2), "Diff predicates does not contains 2")
      assert(contains(i, 4), "Diff predicates does contain an even number")
    }
  }

  test("filter contains shared elements") {
    new TestSets {
      val s = filter(s1, s2)
      val i = filter(s2, twoAndFive)
      assert(!contains(s, 1), "filter singletons does not contain elements from first set")
      assert(!contains(s, 2), "filter singletons does not contain element from second set")
      assert(contains(i, 2), "filter predicates does contain 2")
      assert(!contains(i, 5), "filter predicates does not contain 5")
    }
  }

  test("forall bounded integers within the Set satisfies the predicate") {
    new TestSets {
      val rangeOfEvenNumbers: Set = { x =>
        x == -1000 ||
        x == -100 ||
        x == -2 ||
        x == 4 ||
        x == 200 ||
        x == 1000
      }

      assert(forall(evenNumbers, rangeOfEvenNumbers), "forall evenNumbers satisfies a rangeOfEvenNumbers")
    }
  }

  test("There exists a bounded integer within the Set satisfies the predicate") {
    new TestSets {
      assert(exists(evenNumbers, twoAndFive), "twoAndFive exist in the Set evenNumbers ")
      assert(!exists(evenNumbers, s3), "three does not exist in Set evenNumbers")
    }
  }

  test("`y` exists in `s2` if `x` exists in `s` such that y = f(x)") {
    new TestSets {
      val oddSet: Set = x => x == 1 || x == 3 || x == 5 || x == 7 || x == 9
      val evenSet: Set = x => x == 0 || x == 2 || x == 4 || x == 6 || x == 8
      val doubleEvenSet: Set = x => x == 0 || x == 4 || x == 8 || x == 12 || x == 16

      assert(forall(map(oddSet, x => x - 1), evenSet), "Subtracting 1 from each member of the oddSet produces the evenSet")
      assert(forall(evenSet, map(oddSet, x => x - 1)), "Subtracting 1 from each member of the oddSet produces the evenSet")
      assert(forall(map(evenSet, x => x * 2), doubleEvenSet), "Multiply each member of the evenSet by 2 produces the doubleEvenSet")

    }
  }
}
