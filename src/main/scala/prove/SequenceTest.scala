package prove

import org.junit.Assert.{assertEquals, assertFalse, assertTrue}
import org.junit.Test

class SequenceTest:

  import Sequences.*
  import Sequence.*

  val seq0: Sequence[Int] = Nil()
  val seq = Cons(10, Cons(20, Nil()))
  val seq2 = Cons("10", Cons("20", Nil()))

  @Test def canRun() =
    assertTrue(true)

  @Test def testEmptySequence(): Unit =
    assertTrue(isEmpty(seq0))
    assertFalse(isEmpty(seq))
    assertFalse(isEmpty(seq2))

  @Test def testSum(): Unit =
    assertEquals(0, sum(seq0))
    assertEquals(30, sum(seq))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Nil())), map(seq, x => x + 1))
    assertEquals(Cons(20, Cons(40, Nil())), map(seq, x => x * 2))
    assertEquals(Cons("101", Cons("201", Nil())), map(seq, x => x + "1"))
    assertEquals(seq0, map(seq0, x => x + 1))

  @Test def testFilter() =
    assertEquals(Cons(10, Nil()), filter(seq, x => x == 10))
    assertEquals(Cons(20, Nil()), filter(seq, x => x >= 20))
    assertEquals(seq0, filter(seq0, x => x.equals(Nil())))


