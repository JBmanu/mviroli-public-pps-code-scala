package u06.collections

object Views extends App:

  val list = collection.mutable.ListBuffer(10, 20, 30, 40)
  val view = list.view

  println(view) // SeqView(<not computed>)
  println(view.map(_ + 1))  // SeqView(<not computed>)
  println(view.map(_ + 1).toList) // List(11, 21, 31, 41)

  list.addOne(50)
  println(list) // ListBuffer(10, 20, 30, 40, 50)
  println(view.toList)  // List(10, 20, 30, 40, 50)

  println(view) // SeqView(<not computed>)
  for i <- view do println(i)  // 10 20 30...
  println(view) // SeqView(<not computed>)
