package u07.exports

object Exports extends App:

  trait Counter:
    def value: Int
    def increment(): Unit

  class BasicCounter extends Counter:
    private[Exports] var counterValue = 0
    def value: Int = counterValue
    def increment(): Unit = counterValue = counterValue + 1
    def reset(): Unit = counterValue = 0

  class MultiCounter extends Counter: // no inheritance, just substitutability
    private val counter = new BasicCounter() // no inheritance, just delegation
    export counter.*  // export all, when you just need to add...
    def decrement(): Unit = counterValue -= 1

  class FancyCounter extends Counter: // no inheritance, just substitutability
    private val counter = new BasicCounter() // no inheritance, just delegation
    private val initDate = new java.util.Date() // multiple delegation
    export counter.{reset => _, *}  // export all but few selected methods
    export initDate.{toString => creationTime} // export with alias

  val c: Counter = new FancyCounter() // no inheritance, just substitutability
  println(c.value)
  println(c.increment())
  val c2 = new FancyCounter()
  // println(c2.decrement()) // won't work
  println(c2.creationTime())