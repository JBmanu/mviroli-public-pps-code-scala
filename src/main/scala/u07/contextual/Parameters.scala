package u07.contextual

object Parameters extends App:

  trait MyOrdering[A]:
    def greater(x: A, y: A): Boolean

  def max[T](elem: T, elems: T*)(using ordering: MyOrdering[T]): T =
    elems.foldLeft(elem)((m, e) => if ordering.greater(e,m) then e else m)

  val myStringOrdering = new MyOrdering[String]:
    override def greater(x: String, y: String) = x.compareTo(y) > 0

  println(max("a","b")(using myStringOrdering)) // "b", using a provided ordering

  // I want to use givens, with different styles of definition

  given implicitOrdering: MyOrdering[String] = myStringOrdering
  // without alias would shrink to: given MyOrdering[String] = ...
  // with in-line implementation of the trait with...
  given MyOrdering[Int] with
    def greater(x: Int, y: Int): Boolean = x > y
  // with functional interface to literal abbreviation
  given MyOrdering[List[Int]] = _.size > _.size

  // recovering canonical instance of MyOrdering[String]
  println(summon[MyOrdering[String]])

  println(max("a","b")) // "b", using the given ordering available in scope
  println(max(10, 20)) // 20, using the given ordering available in scope
  println(max(List(10,20), List(30))) // 20, using the given ordering available in scope