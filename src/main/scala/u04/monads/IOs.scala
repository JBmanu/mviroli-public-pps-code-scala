package u04.monads
import Monads.*, Monad.*

object IOs:

  // data structure for a computation with input/output
  case class IO[A](exec: () => A)

  // minimal set of operations 
  object IO:
    def read(): IO[String] = IO(() => scala.io.StdIn.readLine)
    def write[A](a: A): IO[A] = IO(() => { println(a); a })
    def compute[A](a: => A): IO[A] = IO(() => a)
    def nop(): IO[Unit] = compute(())

  // extending IO to be a Monad!
  given Monad[IO] with
    
    // unit: an IO that just returns the boxed value
    def unit[A](a: A): IO[A] = IO(() => a)
    
    // flatMap: opens the box, executes, and creates a new box with result
    extension [A](m: IO[A])
      def flatMap[B](f: A => IO[B]): IO[B] =
        m match
          case IO(e) => f(e())

@main def tryMonadIO =

  // Input-output in Scala, though, imperative!
  val s: String = scala.io.StdIn.readLine
  println(s)
  println(s.toInt)

  // Dealing with input/output in pure FP, by monads
  // Operations with side-effects are hidden in the monad
  import IOs.{*, given}, IO.*

  val simpleInteractiveComputation: IO[Int] = 
    for 
      s <- read()
      _ <- write("Thanks for providing: "+s)
      i <- compute(s.toInt)
      j <- compute(i * 2)
      _ <- write("Result is: "+j)
    yield j  

  // desugared version
  val simpleInteractiveComputationByFlatMaps: IO[Int] = 
    read()                            .flatMap(s => 
    write("Thanks for providing: "+s) .flatMap(_ =>
    compute(s.toInt)                  .flatMap(i =>
    compute(i * 2)                    .flatMap(j =>
    write("Result is: "+j)            .map(_ =>
      j)))))  


import IOs.{*, given}, IO.*
@main def tryDrawGameApp =  
  
  enum Result:
    case Won, Lost

  def drawNumberGame(attempts: Int, draw: Int): IO[Result] =
    if attempts == 0
    then compute(Result.Lost)
    else
      for
        _ <- write("give your number: ")
        d <- read()
        i <- compute(d.toInt)
        res <-
          if (i == draw)
          then
            for
              _ <- write("won")
              r <- compute(Result.Won)
            yield r
          else
            for
              _ <- write(if i > draw then "too high!" else "too low!")
              r <- drawNumberGame(attempts - 1, draw)
            yield r
      yield res
  
  // starts one run of the game
  drawNumberGame(10, scala.util.Random.nextInt(100))

