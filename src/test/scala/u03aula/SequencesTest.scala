package u03aula

import org.junit.Test
import org.junit.Assert.*

class SequencesTest:

  import Sequences.*
  import Sequence.* 
  
  val seq0: Sequence[Int] = Nil()
  val seq = Cons(10, Cons(20, Cons(30, Nil())))  

  @Test def testIsEmpty() =
    assertTrue(isEmpty(seq0))
    assertFalse(isEmpty(seq))

  @Test def testSum() =
    assertEquals(0, sum(seq0))
    assertEquals(60, sum(seq))  

  @Test def testMap() =
    assertEquals(
      Cons(11, Cons(21, Cons(31, Nil()))),
      map(seq, x => x + 1) 
    )
    assertEquals(
      Cons("10.", Cons("20.", Cons("30.", Nil()))),
      map(seq, x => x + ".") 
    )
    assertEquals(
      Cons("a.", Cons("b.", Nil())),
      map(Cons("a", Cons("b", Nil())), x => x + ".") 
    )
    assertEquals(seq0, map(seq0, x => x + 1))
      


