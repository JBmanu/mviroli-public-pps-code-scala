package u07.traits

trait FunctionOption[-A, +B]:
  self =>
  def apply(a: A): Option[B]

  final def andThen[C](f: FunctionOption[B,C]): FunctionOption[A,C] =
    new FunctionOption[A,C]:
      override def apply(a: A): Option[C] =
        self.apply(a).flatMap(b => f.apply(b))
        //this.apply(a).flatMap(b => f.apply(b)) // this would not compile

@main def mainFunction =
  val f: FunctionOption[Int, Int] =
    n => Some(n) filter (_ % 2 == 0) map (_/2)
  val g: FunctionOption[Int, Int] =
    n => Some(n+1)

  val h: FunctionOption[Int, Int] = f andThen g
  println(h(5)) // None
  println(h(6)) // Some(4)