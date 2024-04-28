package u06.collections

object Collections extends App:

  def print[A](coll: Iterable[A]) = println(s"$coll,${coll.getClass}")

  print(Seq(10,20,30,40,50)) // ::
  print(scala.collection.immutable.LinearSeq(10,20,30,40,50)) // ::
  print(IndexedSeq(10,20,30,40,50)) // Vector1
  print(Set(10,20,30,40,50)) // HashSet
  print(scala.collection.immutable.SortedSet(10,20,30,40,50)) // TreeSet
  print(Map(10 -> 11,20 -> 21,30 -> 31,40 -> 41))// HashMap$Map4

  print(Array(10,20,30,40)) //.. mutable.ArraySeq$ofInt

  print(scala.collection.mutable.Seq(10,20,30,40,50)) // ArrayBuffer
  print(scala.collection.mutable.IndexedSeq(10,20,30,40,50)) // ArrayBuffer
  print(scala.collection.mutable.Set(10,20,30,40,50)) // HashSet
  print(scala.collection.mutable.SortedSet(10,20,30,40,50)) // TreeSet
  print(scala.collection.mutable.Map(10 -> 11,20 -> 21,30 -> 31,40 -> 41)) //HashMap
