package u05

object ForComprehension extends App :

  // Basic uses of iterative for
  for i <- 1 to 4 do print(i + " ") // guess what 'to' is..
  println() // 1,2,3,4
  for i <- 1 until 4 do print(i + " ")
  println() // 1,2,3
  (4 to 1 by -1).foreach(i => print(i + " ")) // 4,3,2,1
  println()

  // Custom iterations, with a class and its factory in a companion obj
  class ZeroTo(val n: Int):
    def foreach(action: Int => Unit): Unit = for i <- 0 until n do action(i)

  object ZeroTo:
    def apply(n: Int) = new ZeroTo(n)

  for i <- ZeroTo(10) do print(i + " ")
  println()

  // A variant of for yield: a sequence of flatMap and foreach at the end
  for
    i <- 0 until 10
    if i % 3 == 0
    j <- 0 until 2
  do print(s"$i - $j ")
// 0-0 0-1 3-0 3-1 6-0 6-1 9-0 9-1
