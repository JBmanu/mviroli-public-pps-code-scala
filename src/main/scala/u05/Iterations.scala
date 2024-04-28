package u05

object Iterations extends App:

  def sum(args: Int*): Int =
    var s = 0
    for i <- args do s = s + i
    s

  println(sum(1,2,3,4,5))

  def printall(args: Int*) =
    args foreach (print(_)) // infix notation

  printall(1,2,3,4,5)
  println

  // Arrays are generic classes with proper apply method..
  val a: Array[Int] = Array(1,2,3,4,5,6) // a Java-compatible array
  a(2)=30
  println(s"$a, ${a.apply(1)}, ${a(2)}, ${a.length}")
  // ([I@1a407d53,2,30,6)

  // equivalent code thanks to Scala's apply, update and varargs
  val b: Array[Int] = Array.apply(1,2,3,4,5,6)
  // a Java-compatible array
  b.update(2,30)
  println(s"$b, ${b.apply(1)}, ${b.apply(2)}, ${b.length}")
  // ([I@3d8c7aca,2,30,6)
