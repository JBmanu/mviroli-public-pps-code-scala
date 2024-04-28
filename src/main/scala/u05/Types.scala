package u05

@main def typesExample() =  // a top-level main method

  val a: AnyVal = 1.2
  val b: Any = Object()
  val c: Null = null
  val e: java.util.Date = null // null is a Java thing..
  // val n: Nothing = throw new Error()
  // val iterable: Nothing = ??? // scala.NotImplementedError

  val i = 10
  val j = 10 + 20
  val k = i.+(20) // binary arguments are just method calls

  val f: Function1[Int, Int] = x => x+1  // package scala imported by default
  val g: Int => Int = f // Function1[Int,Int] an alias for (Int)=>(Int)
  println(f) // <function1>
  println(f(1)) // 2
  println(f.apply(1)) // 2, calling a function is invocation of apply
  println((f compose g compose f)(1)) // 4
  println((f.compose(g.compose(f)))(1)) // 4

  val f2: Function2[Int, Int, Int] = _ + _
  println(f2) // <function2>
  println(f2(1, 2)) // 3