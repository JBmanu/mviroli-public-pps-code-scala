package u04.typeclasses

import u03.extensionmethods.Sequences.*
import u03.extensionmethods.Optionals.*
import Sequence.*

object ContextBound:

  // Ordered as a generic module type, namely, a so called "type class"
  trait Ordered[T]:
    def greater(t1: T, t2: T): Boolean

  // using-oriented style of definition
  def maxWithUsing[T](seq: Sequence[T])(using ordered: Ordered[T]): T = max(seq)

  // equivalent definition with context bound, recalling "ad-hoc polymorhism"
  // it can be called on T only if an Ordered[T] is available in scope
  def max[T: Ordered](seq: Sequence[T]): T = seq match
    case Cons(h1, Cons(h2, t)) => max(Cons(if summon[Ordered[T]].greater(h1, h2) then h1 else h2, t))
    case Cons(h1, Nil()) => h1
  
  // defining an Ordered "extension" for type Int
  given Ordered[Int] with
    def greater(t1: Int, t2: Int): Boolean = t1 > t2

  // doing the same for String, in a different style
  object MyOrderedString extends Ordered[String]:
    def greater(t1: String, t2: String): Boolean = t1 > t2  
  given Ordered[String] = MyOrderedString

  @main def tryContextBound =  
    // the necessary given are already in scope
    println:
      max(Cons(10, Cons(30, Cons(20, Nil()))))
    println:
      max(Cons("a", Cons("c", Cons("d", Nil()))))  
    // println(max(Cons(1.0, Cons(3.0, Cons(5.0, Nil()))))) // not working