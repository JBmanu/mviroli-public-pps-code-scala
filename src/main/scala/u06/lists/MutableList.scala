package u06.lists

trait MutableList[A]:
  def head: Option[A]
  def tail: Option[MutableList[A]]
  def prepend(h: A): MutableList[A]
  def append(l: MutableList[A]): MutableList[A]
  def map[B](f: A => B): MutableList[B]
  def toList: List[A]

object MutableList:

  def apply[A](): MutableList[A] = MutableListImpl(None)

  private case class MutableListImpl[A](
      var ht: Option[(A, MutableList[A])]) extends MutableList[A]:

    override def head: Option[A] = ht.map(_._1)

    override def tail: Option[MutableList[A]] = ht.map(_._2)

    override def prepend(h: A): MutableList[A] =
      ht = Some((h, MutableListImpl(ht)))
      this

    override def append(l: MutableList[A]): MutableList[A] =
      ht match
        case None if l.head.isEmpty => {} // [],[] --> []
        case None => ht = Some((l.head.get, l.tail.get)) // [],[H|T] --> [H|T]
        case Some((h, t)) if t.head.isEmpty => ht = Some((h, l)) // [H],L --> [H|L]
        case Some((h, t)) => ht = Some((h, t.append(l))) // [H|T],L --> [H|T.append(L)]
      this

    override def map[B](f: A => B): MutableList[B] =
      MutableListImpl(ht.map((h, t) => (f(h), t.map(f))))

    override def toList: List[A] = ht match
      case None => List.Nil()
      case Some((h, t)) => List.::(h, t.toList)


object TestMutableList extends App:

  import MutableList.*

  val l: MutableList[Int] = MutableList()
  l.prepend(40).prepend(30).prepend(20).prepend(10)
  println(l.toList) // 10,20,30,40
  println(l.head) // Some(10)
  println(l.tail.get.toList) // 20,30,40
  val l2: MutableList[Int] = MutableList()
  l2.prepend(60).prepend(50)
  l.append(l2) // what would happen with l.append(l) ???
  println(l.toList) // 10,20,30,40,50,60
  println(l.map(_ + 1).toList) // 11,21,31,41,51,61