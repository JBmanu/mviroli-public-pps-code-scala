package u07.generics

object Variance extends App:

  /*
  package scala.*
  abstract class Option[+A]
  trait PartialFunction[-T1, +R] // { case x => ... }
  trait Function0[+R]       // ()=>R
  trait Function1[-T1, +R]  // (T1)=>R
  trait Function2[-T1, -T2, +R] // (T1,T2)=>R
  case class Tuple2[+T1, +T2](_1: T1, _2: T2) // (T1,T2)

  package scala.collection
  trait Iterator[+A]
  trait Set[A] // ... extends (A) => Boolean
  trait Seq[+A] extends PartialFunction[Int, A]... extends (Int) => A
  trait Map[K, +V] extends PartialFunction[K, V]... extends Iterable[(K,V)]
  */

  val v: Option[List[Any]] = Some[List[Int]](List(10,20,30))
  val w: Option[Iterable[Any]] = Some[Set[Int]](Set(10,20,30))
  val f: String => Any = (s:Any) => s.toString
  val s: String => Boolean = Set("a", "b", "c")
  val seq: Int => String = List("a", "b", "c")
  val m: String => Int = Map( "a" -> 1, "b" -> 2, "c" -> 3)

