package u06.collections

object UseMaps extends App:

  // an immutable map, i.e., a key-value store
  val m: Map[Int,String] = Map(10 -> "a",20 -> "b",30 -> "c")
  println(m.getClass) // class scala.collection.immutable.Map$Map3
  println(m) // Map(10 -> a, 20 -> b, 30 -> c)
  println(m.toVector) // Vector((10,a), (20,b), (30,c))
  println(m.values)   // Iterable(a, b, c)
  println(m.keys)     // Set(10, 20, 30)
  println(m(10)) // "a"
  println(m.map{ case (k,v) => (v,k+".")}) // Map(a -> 10., b -> 20., c -> 30.)
  println(m.map{ case (k,v) => v+v}) // List(aa, bb, cc)

  // a mutable map
  import collection._
  val mm: mutable.Map[Int,String] = mutable.Map[Int,String]()
  println(mm.getClass) // class scala.collection.mutable.HashMap
  println(mm) // Map()
  mm += (10 -> "a")
  mm += (20 -> "b")
  mm += (30 -> "c")
  println(mm) // Map(20 -> b, 10 -> a, 30 -> c)
