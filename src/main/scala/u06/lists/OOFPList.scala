package u06.lists

// List as a pure interface
enum List[A]:
  case ::(h: A, t: List[A])
  case Nil()
  def ::(h: A): List[A] = List.::(h, this)

  def head: Option[A] = this match
    case h :: t => Some(h)  // pattern for scala.Option
    case _ => None          // pattern for scala.Option

  def tail: Option[List[A]] = this match
    case h :: t => Some(t)
    case _ => None

  def append(list: List[A]): List[A] = this match
    case h :: t => h :: t.append(list)
    case _ => list

  def foreach(consumer: A => Unit): Unit = this match
    case h :: t => consumer(h); t.foreach(consumer)
    case _ =>

  def get(pos: Int): Option[A] = this match
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t.get(pos - 1)
    case _ => None

  def filter(predicate: A => Boolean): List[A] = this match
    case h :: t if predicate(h) => h :: t.filter(predicate)
    case _ :: t => t.filter(predicate)
    case _ => Nil()

  def map[B](fun: A => B): List[B] = this match
    case h :: t => fun(h) :: t.map(fun)
    case _ => Nil()

// Factories
object List:

  def apply[A](elems: A*): List[A] =
    var list: List[A] = Nil()
    for e <- elems.reverse do list = e :: list
    list

  def of[A](elem: A, n: Int): List[A] =
    if n == 0 then Nil() else elem :: of(elem, n - 1)

object Test extends App:

  import List.*

  println(List(10, 20, 30, 40))
  val l = 10 :: 20 :: 30 :: 40 :: Nil() // same as above
  println(l.head) // Some(10)
  println(l.tail) // Some(...20,30,40...)
  println(l.append(l)) // 10,20,30,40,10,20,30,40
  println(l append l) // 10,20,30,40,10,20,30,40
  println(l get 2) // Some(30)
  println(of("a", 10)) // a,a,a,..,a
  println(l filter (_ <= 20) map ("a" + _)) // a10, a20
  println(l.filter(_ <= 20).map("a" + _)) // a10, a20
  println(for i <- l do println(i)) // uses foreach

  println(l.append(l) == l) // false, note intrinsic immutability