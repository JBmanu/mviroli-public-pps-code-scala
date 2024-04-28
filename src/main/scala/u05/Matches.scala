package u05

object Matches extends App:

  class Pair[A,B](val x: A, val y: B)

  object Pair:
    def apply[A,B](x: A, y: B) = new Pair(x,y)
    def unapply[A,B](p: Pair[A,B]): Option[(A,B)] = Some((p.x,p.y))

  class MyPair(override val x: Double, override val y: Double)
    extends Pair[Double,Double](x,y)

  object OfLength: // extracting distance to 0 when greater than 1
    def unapply(p: Pair[Double,Double]): Option[Double] =
      Some(Math.sqrt(p.x*p.x+p.y*p.y)) filter (_>1)

  val z = 2
  def process(p: Pair[Double,Pair[Double,Double]]): String = p match
    case Pair(1,Pair(1,1)) => "A"    // matching a complex structure, defined
    case Pair(2,Pair(0,_)) => "B"    // matching a complex structure, partially specified
    case Pair(3,null) | Pair (4,null) => "C" // or
    case Pair(5,Pair(x,y)) if x==y => "D"  // conditional case
    case Pair(6, pair @ Pair(_,_)) if pair.y == 1  => "E" // capturing variable
    case Pair(7,Pair(`z`,`z`)) => "F"+z // logical binding to 0,0,0
    case Pair(8,OfLength(n)) => "G"+n  // usign a custom extractor
    case Pair(9,p: MyPair) => "H"  // usign a type check
    case _ => "?" // default case

  println(process(Pair(1,Pair(1,1)))) // A
  println(process(Pair(2,Pair(0,0)))) // B
  println(process(Pair(3,null))) // C
  println(process(Pair(5,Pair(1,1)))) // D
  println(process(Pair(6,Pair(1,1)))) // E
  println(process(Pair(7,Pair(1,2)))) // ?
  println(process(Pair(7,Pair(2,2)))) // F2
  println(process(Pair(8,Pair(1,1)))) // G1.4....
  println(process(Pair(8,Pair(0.2,0.2)))) // ?
  println(process(Pair(9,new MyPair(10,10)))) // H
