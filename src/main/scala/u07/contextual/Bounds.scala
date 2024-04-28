package u07.contextual

object Bounds extends App {

  // an alternative implementation of max, with a CONTEXT BOUND
  // max2 requires T to have an implicit Ordering[T] available
  // Ordering is a scala equivalent of Comparator
  def max2[T: Ordering](t1: T, t2: T): T =
    implicitly[Ordering[T]].max(t1,t2)

  println(max2("a","b"))

  /*
  // an alternative implementation of max, with a VIEW BOUND
  // max3 requires T to be convertible to Ordered[T]
  // Ordered is a scala equivalent of Comparable
  def max3[T : Ordered](t1: T, t2: T): T =
    if (t1 gt t2) t1 else t2

  println(max3("a","b"))
  */

}
