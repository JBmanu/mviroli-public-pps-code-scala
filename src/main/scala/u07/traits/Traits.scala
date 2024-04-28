package u07.traits

trait T {
  def f:String = "T"
}

trait A extends T {
  val x = "A"
  override def f:String = super.f + x
}

trait B extends T {
  override def f:String = super.f + "B"
}

// Linearization creates top-down the stack: TABC
class C extends A with B {

}

object Traits extends App {

  println(new C().f) // TAB
}
