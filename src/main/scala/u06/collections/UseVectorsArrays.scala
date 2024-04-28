package u06.collections

object UseVectorsArrays extends App:
  // Indexed sequences have constant access time to elements and size

  // Vector is the default immutable indexed sequence
  val v: Vector[Int] = Vector(10,20,30,40,50)
  println(v.getClass) // class scala.collection.immutable.Vector1
  // it supports all operations of lists..
  println(v.reverse) // Vector(50, 40, 30, 20, 10)
  println(v(2)) // 30

  // scala.Array is backened by Java arrays!
  // Array is a mutable indexed sequence, with constant length
  val a: Array[Int] = Array(10,20,30)
  println(a) // [I@7f690630
  println(a.getClass) // class [I, namely an int[] !!
  // it supports all operations of lists..
  println(a.reverse) // [I@6bf256fa
  println(a.reverse.toList) // List(30, 20, 10)

  import collection.*
  // ArrayBuffer is the default (fully) mutable indexed sequence
  val b: mutable.ArrayBuffer[Int] = mutable.ArrayBuffer[Int]()
  println(b) // ArrayBuffer
  println(b.getClass) // class scala.collection.mutable.ArrayBuffer
  b.addAll(List(10,20,30,40))
  println(b)  // ArrayBuffer(10, 20, 30, 40)
  println(b.toArray) // [I@62043840
  println(b.toVector) // to an immutable sequence
