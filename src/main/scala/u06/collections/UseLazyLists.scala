package u06.collections

object UseLazyLists extends App:

  // list is an immutable linear sequence, optimising head and tail access
  val s: LazyList[Int] = LazyList(10,20,30,40,50) // works without imports
  println(s.getClass) // class scala.collection.immutable.LazyList
  println(s.isInstanceOf[scala.collection.immutable.LazyList[_]]) // true
  println(s) // LazyList(<not computed>)
  println(s(2)) // 30
  println(s) // LazyList(10, 20, 30, <not computed>)
  val s2 = s.zipWithIndex
  println(s2) // LazyList(<not computed>)
  println(s2(2)) // 30
  println(s2) // LazyList((10,0), (20,1), (30,2), <not computed>)
  println(s.toList) // List(10, 20, 30, 40, 50)
  println(s) // LazyList(10, 20, 30, 40, 50)