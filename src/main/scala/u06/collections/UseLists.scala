package u06.collections

object UseLists extends App:

  // list is an immutable linear sequence, optimising head and tail access
  val l: List[Int] = List(10,20,30,40,50) // works without imports
  println(l.getClass) // class scala.collection.immutable.$colon$colon
  println(l.isInstanceOf[scala.collection.immutable.List[_]]) // true
  println(l) // List(10,20,30,40,50)
  println(l.reverse) // List(50, 40, 30, 20, 10)
  println(l.zipWithIndex) // List((10,0), (20,1), (30,2), (40,3), (50,4))
  println(l.splitAt(3)) // a tuple.. (List(10, 20, 30),List(40, 50))
  println(l(2)) // 30
  println(5 :: l) // List(5, 10, 20, 30, 40, 50)
  println(l ::: l) // List(10, 20, 30, 40, 50, 10, 20, 30, 40, 50)
  println(l :+ 51) // List(10, 20, 30, 40, 50, 51)
  println((l :+ 11).sortWith(_<_)) // List(10, 11, 20, 30, 40, 50)
  println(l collect {case i if i > 20 => i + 1})  // List(31, 41, 51)
  println(List.range(0,10)) // List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  println(List.iterate(0,5)(_+3)) // List(0, 3, 6, 9, 12)

  // working with mutable lists: a buffer for lists
  import scala.collection.mutable.ListBuffer
  val fruits = ListBuffer[String]() // mutable buffer to list
  fruits += "Apple"  // means: fruits.+=("Apple")
  fruits += "Banana"
  fruits += ("Strawberry", "Kiwi")
  fruits -= "Apple"
  println(fruits) // ListBuffer(Banana, Strawberry, Kiwi)
  println(fruits.toList) // List(Banana, Strawberry, Kiwi)

