package u06.lists

object RightAssociativeConstruction extends App:

  enum List[A]: // a type with two implementations and methods
    case ::(h: A, t: List[A]) // just a different name for Cons
    case Nil()
    def ::(h: A): List[A] = List.::(h, this)
    def isEmpty: Boolean = this == Nil() // another method

  object List:
    def head(l: List[Int]): Option[Int] = l match
      case h :: _ => Some(h) // same as ::(h, _) => Some(h)
      case _ => None

  import List.*

  val l: List[Int] = ::(10, ::(20, ::(30, Nil()))) // using case ::(h, t)
  val l2: List[Int] = Nil().::(30).::(20).::(10)   // using def ::
  val l3: List[Int] = 10 :: 20 :: 30 :: Nil()      // using infix notation
  // recall that operators ending with : have right-associativity

  println(s"$l, $l2, $l3, ${l.isEmpty}")
  println(head(l)) // FP-like call
  println(l.isEmpty) // OO-like call
