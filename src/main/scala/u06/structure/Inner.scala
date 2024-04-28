package u06.structure

object Inner extends App:

  class C(var v: Int):
    class D:
      def g(n: Int): Int =
        n + C.this.v // using outer instance

  val c: C = new C(5) // enclosing instance
  println(new c.D().g(4)) // 9

  object O:
    var v: Int = 10
    class D: // a static inner class
      def g(n: Int): Int =
        n + O.v

  println(new O.D().g(4)) // 14