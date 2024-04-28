package u07.comprehension

object Comprehension extends App:

  val l = List(10,20,30)
  println(l map (_ * 2))            // 20,40,60
  println(l flatMap (List(_, 0)))  // 10,0,20,0,30,0
  println(l filter (_ > 10))        // 20,30

  // Problem: give all quartets (x,y,z,x*y*z) such that:
  // 1) x,y,z, in 1..9; 2) the sum of x,y,z gives 10; 3) x < y < z

  val l2 = for
    x <- 1 until 9
    y <- 1 until 9
    z <- 1 until 9
    if x + y + z == 10
    if x < y && y < z
    w = x * y * z
  yield
    (x, y, z, w)
  println(l2) // Vector((1,2,7,14), (1,3,6,18), (1,4,5,20), (2,3,5,30))

  // desugared version (Scala 2)
  val l3 = (1 until 9)
    .flatMap( x => (1 until 9)
      .flatMap( y => (1 until 9)
        .filter( z => x + y + z == 10 )
        .filter( z => x < y && y < z )
        .map( z => (z, x * y * z))
        .map{ case (z, w) => (x, y, z, w)}))
  println(l3) // Vector((1,2,7,14), (1,3,6,18), (1,4,5,20), (2,3,5,30))