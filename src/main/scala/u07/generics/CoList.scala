package u07.generics

// alternative approach: a sealead trait and case classes/objects
// sealed trait CoList[+T]
// case class Cons[+T](head: T, tail: CoList[T]) extends CoList[T]
// case object Nil extends CoList[Nothing]  // a single-object case

enum CoList[+T]:
  case Cons(head: T, tail: CoList[T])
  case Nil

object CoList:
  def apply[T](elems: T*): CoList[T] =
    elems.foldRight[CoList[T]](Nil)(Cons(_, _)) // functional trick

object TryCoList extends App:

  // would not be typed with CoList[T] instead of CoList[+T]
  val ll: CoList[CoList[Any]] = CoList(
    CoList(10,20,30),
    CoList("a", "b", "c"),
    CoList(10, new java.util.Date, "a string"))
  println(ll)