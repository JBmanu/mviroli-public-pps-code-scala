package u07.traits

trait Ordered[T] extends Comparable[T]:
  def compareTo(that: T): Int
  def <(that: T): Boolean = (this compareTo that) < 0
  def >(that: T): Boolean = (this compareTo that) > 0
  def <=(that: T): Boolean = (this compareTo that) <= 0
  def >=(that: T): Boolean = (this compareTo that) >= 0

case class MyInt(n:Int) extends Ordered[MyInt]:
  override def compareTo(that: MyInt) = this.n - that.n

object TryOrdered extends App:
  println( MyInt(5) < MyInt(7))
  println( MyInt(5) <= MyInt(7))
  println( MyInt(5) > MyInt(7))
  println( MyInt(5) >= MyInt(7))

// package scala has Ordered[T] (for Comparable)
// package scala has Ordering[T] (for Comparator)
