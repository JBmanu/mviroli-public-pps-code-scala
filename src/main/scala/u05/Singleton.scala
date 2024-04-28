package u05

object Singleton extends App :

  trait SequenceGenerator:
    def get: Int

  class SequenceFromSupplier(f: () => Int) extends SequenceGenerator:
    override def get = f()

  val s: SequenceGenerator = new SequenceFromSupplier(() => 1)
  println(s.get)
  println(s.get)

  object Zeros extends SequenceFromSupplier(() => 0)

  object GlobalCounter extends SequenceGenerator:
    private var n: Int = 0
    override def get: Int = 
      n = n + 1
      n - 1

  val s2: SequenceGenerator = GlobalCounter
  println(s2.get)
  println(s2.get)
  println(GlobalCounter.get)
