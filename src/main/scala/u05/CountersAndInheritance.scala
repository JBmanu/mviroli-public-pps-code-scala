package u05

object CountersAndInheritance:

  trait Counter:
    def value:Int
    def increment(): Unit

  class CounterImpl(protected var _value: Int) extends Counter:
    override def value: Int = _value
    override def increment(): Unit = _value = _value + 1

  trait DecrementCounter extends Counter:
    def decrement(): Unit

  class DecrementCounterImpl() extends CounterImpl(0) with DecrementCounter:
    override def decrement(): Unit = _value = _value - 1

  @main def main() =
    val c: Counter = new CounterImpl(0)
    c.increment()
    c.increment()
    println(c.value)
    val c2: DecrementCounter = new DecrementCounterImpl()
    c2.decrement()
    println(c2.value)

