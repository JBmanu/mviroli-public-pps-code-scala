package u06.structure

class C: // a class with many private properties
  def f(): Unit =
    g1()
    g2()
    g3()
  private def g1(): Unit = { ??? } // local helpers
  private def g2(): Unit = { ??? }
  private def g3(): Unit = { ??? }

class C2: // a refactoring of C, nesting implementation code
  import ImplementationHelpers.* // to facilitate calls

  def f(): Unit =
    g1()
    g2()
    g3()

  private object ImplementationHelpers:
    def g1(): Unit = { ??? }
    def g2(): Unit = { ??? }
    def g3(): Unit = { ??? }
