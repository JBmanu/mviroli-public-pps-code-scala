package u07.aliases

// an extract of scala.Predef
class Predef:
  type Class[T] = java.lang.Class[T]
  type Function[-A, +B] = (A) => B
  type Map[A, +B] = collection.immutable.Map[A, B]
  type Set[A] = collection.immutable.Set[A]
  type String = java.lang.String

// aliases and corresponding constructors
object MyAliases:
  type MySeq[T] = collection.immutable.Vector[T]
  def MySeq[T](elems: T*) = collection.immutable.Vector(elems)
  type MySet[T] = collection.immutable.HashSet[T]
  def MySet[T](elems: T*) = collection.immutable.HashSet(elems)
  type MyMap[A, +B] = collection.immutable.HashMap[A, B]
  def MyMap[A, B](elems: (A, B)*) = collection.immutable.HashSet(elems)

object Aliases extends App :
  import MyAliases.* // using aliases
  val seq = MySeq(10, 20, 30)
  println(s"$seq,${seq.getClass}")
  val set = MySet(10, 20, 30)
  println(s"$set,${set.getClass}")
  val map = MyMap(10 -> "a", 20 -> "b", 30 -> "c")
  println(s"$map,${map.getClass}")