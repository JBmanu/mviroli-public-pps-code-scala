package u07.comprehension

object Nqueen extends App:
  type Queen = (Int, Int) // Type aliases improve readability
  type Solution = Iterable[Queen]
  type IterableFactory = Solution => Iterable[Solution]
  val size = 10
  given IterableFactory = LazyList(_) // could switch to Set(_), List(_), or List(_).view
  
  @main def run() = placeQueens().zipWithIndex foreach (printSolution(_))

  def placeQueens(n: Int = size)(using factory: IterableFactory): Iterable[Solution] = n match
    case 0 => factory(Set())
    case _ =>
      for
        queens <- placeQueens(n - 1)  // get a solution on a smaller grid
        y <- 1 to size                // iterate over all columns
        queen = (n, y)                // consider a new queen on any cell in last row
        if isSafe(queen, queens)      // ...only a safe one
      yield
        queens.toSeq :+ queen               // construct the new solution

  def isSafe(queen: Queen, others: Iterable[Queen]) =
    others forall (!isAttacked(queen, _))

  def isAttacked(q1: Queen, q2: Queen) =
    q1._1 == q2._1 || q1._2 == q2._2 || (q2._1 - q1._1).abs == (q2._2 - q1._2).abs

  def printSolution(si: (Solution, Int)): Unit =
    println(); println(s"sol ${si._2}")
    for queen <- si._1; x <- 1 to size do
      print(if queen._2 == x then "Q " else ". ")
      if x == size then println()