package u06.collections

object UseOption extends App :

  val some: Option[Int] = Some(10)
  val none: Option[Int] = None

  println(some.isDefined) // true

  println(some.fold(-1)(_ + 1)) // 11
  println(none.fold(-1)(_ + 1)) // -1

  println(some map (_ + 1)) // Some(11)
  println(none map (_ + 1)) // None

  println(some filter (_ > 0)) // Some(10)
  println(none filter (_ > 0)) // None
  println(Some(-5) filter (_ > 0)) // None

  println(some.flatMap(x => if x > 0 then Some(1) else None)) // Some(1)
  println(some.flatMap(x => Some(1) filter (i => x > 0))) // Better than the above
  println(none.flatMap(x => Some(0) filter (i => x > 0))) // None

  val some2: Option[Int] = Some(20)
  println(some flatMap (x => some2 flatMap (y => Some(x * y)))) // Some(200)

