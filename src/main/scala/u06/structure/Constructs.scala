package u06.structure

object Constructs extends App:

  // nestable constructs
  class C
  abstract class D
  object O
  trait T
  def f():Unit = {}

  println(s"${new C}, $O, ${f()}")
  val d = new D(){}   // essentially, anonymous classes
  val t = new T{}     // essentially, anonymous classes
  println(s"$d, $t, ${new D(){}}, ${new T{}}")

  // nestable constructs with some content (in Scala 2 syntax)
  class C2 { def a: Int = 1 }
  abstract class D2 { def b: Int}
  object O2 { def c: Int = 3 }
  trait T2 { def d(): Int } // a function interface
  def f2():Unit = print("hello")
  val d2 = new D2():  // anonymous class
    override def b = 2
  val t2: T2 = () => 4    // Java-like fill of functional interface

  println(s"${new C2().a}, ${d2.b}, ${O2.c}, ${f2()}, ${t2.d()}")
