package u05.pack  // the package of all definitions below

val v1 = 1 // when is it initialised?

package a:   // entering a subpackage as a namespace
  object O1
  package b: // entering a further subpackage as a namespace
    def f2() = 1
    object O2
    object A:  // I keep nesting stuff
      object O3
      object B:
        object O4
        class C:
          println(s"$O1,$O2,$O3,$O4,$A,$B,${f()}") // note visibility on top
          println(s"${u05.pack.a.b.A.B.O4}") // full qualification
        def f():Unit =
          println(s"$O1,$O2,$O3,$O4,$A,$B,${new C},$v1,${f2()}")

package a:   // re-entering package u05.pack.a
  object O1B:
    val v = O1 // using u05.pack.a.O1

object Packages extends App:
  println(u05.pack.a.b.A.B.O4)
  println(u05.pack.a.O1B)