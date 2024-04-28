package u06.collections

object UseSets extends App:

  // an immutable set, i.e., no sequencing and no duplications
  val s: Set[Int] = Set(10,20,30,40,50)
  println(s.getClass) // class scala.collection.immutable.HashSet$HashTrieSet
  println(s) // Set(10, 20, 50, 40, 30)
  // it supports most operations of lists.. and also..
  println(s + 60 + 70) // Set(10, 20, 60, 70, 50, 40, 30)
  println(s & Set(10,30)) // intersection.. Set(10)
  println(s.contains(10)) // containment of an element

  import collection._
  // mutable set, aliasing a HashSet
  val b: mutable.Set[Int] = mutable.Set(10,20,30)
  println(b.getClass) // class scala.collection.mutable.HashSet
  println(b) // Set(30, 20, 10)
  b += 40    // means b.+=(40)
  b += 40
  println(b) // Set(30, 20, 10, 40)

