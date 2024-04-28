package u07
// libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"
// check details at: https://www.scalatest.org/

import org.scalatest.matchers.should.Matchers.*

class ScalaTestShowcase extends org.scalatest.funsuite.AnyFunSuite:
  
  test("List operations, and should matchers"):
    val testedList: List[Int] = 10 :: 20 :: 30 :: 40 :: Nil
    assert(testedList.size == 4) // JUnit-like assertion
    testedList.size shouldBe 4   // from here, should matchers...
    testedList.size should be (4)
    testedList should have size 4
    testedList should not be empty
    testedList.forall(_ > 5) should be (true)
    a [IndexOutOfBoundsException] should be thrownBy testedList(5)
    testedList.collect{case i if i > 20 => i + 1} should contain allOf (31, 41)
    "val l = 10 :: 20 :: Nil" should compile

class SetSpec extends org.scalatest.flatspec.AnyFlatSpec:

  "A mutable Set" should "correctly add elements" in:
    val s: collection.mutable.Set[Int] = collection.mutable.Set()
    s += 10
    s += 20
    s += 30
    assert(s == Set(10, 20, 30))
    s should be (Set(10, 20, 30))
