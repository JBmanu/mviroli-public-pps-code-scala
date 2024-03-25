package u04.adts

object EuroADT:
  opaque type Euro = Int // an alias, into this module, opaque outside

  def fromEuroCents(euros: Int, cents: Int): Euro =
    require(euros >= 0 && cents >= 0)
    cents + euros * 100

  def fromDouble(d: Double): Euro =
    require(d >= 0 && d <= 1_000_000)
    (d * 100).toInt // extension method in libs

  extension (e: Euro)
    def asString(): String = "\u0024%d.%2d".format(e / 100, e % 100)
    def sum(e2: Euro): Euro = e + e2

@main def tryEurosADT =
  import EuroADT.*
  
  val e1: Euro = fromEuroCents(100, 50)
  val e2: Euro = fromDouble(100.0 / 3)
  println(e1.asString()) // E100.50
  println(e2.asString()) // E33.33
  println((e1 sum e2).asString()) // E133.83
  // val e3: Euro = 100 // not allowed because of opaqueness!
  // val e3: Euro = fromEuroCents(-10, 0) // illegal argument exception