package u06.collections

object WorkingWithIterable extends App:

  class Fibonacci(n: Int) extends Iterable[Int]:
    override def iterator: Iterator[Int] = new Iterator[Int]():
      var counter = 0
      var prev = (0, 1)
      override def next(): Int =
        prev = (prev._2, prev._1 + prev._2)
        counter = counter + 1
        prev._1
      override def hasNext: Boolean = counter <= n

  val fibs = Fibonacci(20)
  val fibsAsStrings = fibs.map(i => s"Next one is $i")
  val list = fibs.toList

