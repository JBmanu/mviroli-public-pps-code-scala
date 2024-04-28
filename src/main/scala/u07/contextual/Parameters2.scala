package u07.contextual

object Parameters2 extends App:

  trait MyOrdering[A]:
    def greater(x: A, y: A): Boolean

  // equivalent to previous formulation, using a env bound
  def max[T: MyOrdering](elem: T, elems: T*): T =
    elems.foldLeft(elem)((m, e) => if summon[MyOrdering[T]].greater(e,m) then e else m)


  object GivenOrderings: // all shrinkable to literals
    given MyOrdering[String] with
      override def greater(x: String, y: String) = x.compareTo(y) >= 0
    given MyOrdering[Int] with
      override def greater(x: Int, y: Int) = x>=y
    // a generic way of expressing given terms, alias needed
    given listOrdering[A]: MyOrdering[List[A]] with
      override def greater(x: List[A], y: List[A]) = x.size >= y.size

  import GivenOrderings.given

  println(max("a","d","b","c")) // "d"
  println(max(10,40,20,30)) // 40
  println(max(List(10,20),List(30,40,50),List("a","b"))) // List(30,40,50)
