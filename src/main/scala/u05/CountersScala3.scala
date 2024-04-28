package u05

object CountersScala3:
  trait Counter:
    def increment(): Unit
    def value: Int  // typical style of a getter

  class CounterImpl extends Counter:
    private var _value: Int = 0   // style of fields when equal to getters

    override def increment() =    // return type of an override can be omitted
      _value = _value + 1         // syntax "this." could be omitted
  
    override def value: Int = _value

  @main def tryCounters() = 
    val counter: Counter = new CounterImpl  // "()" typically always optional
    counter.increment()
    counter.increment()
    counter.increment()
    println("Result is " + counter.value) // 3
