package u05

// A trait defines a type, it includes a contract and possibly an incomplete class
trait Counter {
  def increment(): Unit // Unit is the equivalent of "void"
  def getValue(): Int
}

// a class with no constructors, i.e., only default constructor
class CounterImpl extends Counter {

  private var value: Int = 0 // a mutable field

  // public is the default visibility
  // override is a keyword, mandatory for actual overriding
  override def increment(): Unit = {  // return type could be omitted
    this.value = this.value+1 // operator ++ not available...
  }

  override def getValue(): Int = {
    return this.value // return keyword not needed, but possible
  }
}

// a module, with a main
object UseCounter { // note an object could extend a class..
  def main(args: Array[String]) = {   // Array as a standard generic class
    val counter: Counter = new CounterImpl()  // "()" typically always optional
    counter.increment()
    counter.increment()
    counter.increment()
    println("Result is "+counter.getValue()) // 3
  }
}
