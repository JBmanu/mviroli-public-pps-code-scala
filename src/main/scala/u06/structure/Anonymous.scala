package u06.structure

object Anonymous extends App:

  trait Comparator[A]:
    def compare(a: A, b: A): Int

  // a function inside a function
  def min[A](list: List[A], comp: Comparator[A]): Option[A] =
    @annotation.tailrec
    def min2(list: List[A], minSoFar: A): A = list match {
      case h :: t if (comp.compare(h, minSoFar) > 0) => min2(t, minSoFar)
      case h :: t => min2(t, h)
      case _ => minSoFar
    }
    //if (list.isEmpty) None else Some(min2(list.tail, list.head))
    Some(list) filter (! _.isEmpty) map (l => min2(l.tail, l.head))

  // Instantiating an anonymous class
  val comp = new Comparator[Int]():
    override def compare(a: Int, b: Int): Int = a - b

  println(min(List(10,20,5,40), comp)) // Some(5)
  println(min(List(), comp)) // None
  println(min(List(10,20,5,40), _ - _)) // fill of functional interface
