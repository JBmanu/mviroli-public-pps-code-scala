package u05.structure

package pa1:
  package pa2:

    class C:
      private object pri            // visible only by C's instances
      private[pa2] object pri_pa2   // visible only up to pa2
      protected object pro          // visible to subclasses
      protected[pa2] object pro_pa2 // visible to subclasses, or up to pa2
      def f(c: C) =
        print(C.g())  // WORKS, can see companion's private stuff

    object C:
      print(new C().pri)         // WORKS, can see class' private stuff
      private def g() = 1

    class D extends C
      //print(pri) // ALL ERRORS

    class E(c: C):
      //print(c.pri,c.pro) // ALL ERRORS
      print(c.pri_pa2, c.pro_pa2) // WORKS!
  end pa2

  class F extends pa2.C:
    //print(pri_pa2) // ALL ERRORS
    print(pro_pa2) // WORKS!

end pa1


