package u07.contextual

object Conversions extends App:
  // an example class and a conversion defined in its companion
  case class Point(n: Int, m: Int)
  object Point: // the companion object is a proper place for conversions
    // specifying a conversion from tuple to point, by apply method
    given Conversion[(Int, Int), Point] with
      def apply(t: (Int,Int)): Point =
        println("converting tuple " + t + " to a point..") // a debug print
        Point(t._1, t._2)

    // specifying a conversion by a literal
    given Conversion[List[Int], Point] = l => Point(l.head, l.tail.head)

  def pointToRadius(p: Point) = Math.sqrt(p.n * p.n + p.m * p.m)

  // the given conversion is already visible as in Point companion
  println(pointToRadius(Point(3, 4))) // 5

  val tup = (3,4)
  println(pointToRadius(tup)) // 5
  println(pointToRadius(List(3,4))) // 5
  // try desugaring with Ctrl-Alt-D in IntelliJ

  // a facility to check (e.g. in REPL) if a given conversion is active
  println(summon[((Int,Int)) => Point]( (10,20) )) //Point(10,20)
  println(summon[List[Int] => Point]( List(20,30,40) )) //Point(10,20)
  println(summon[Nothing => Point]( List(20,30,40) )) //Point(10,20)