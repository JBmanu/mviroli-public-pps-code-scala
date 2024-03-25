package u03

object Streams extends App:

  import Sequences.*

  enum Stream[A]:
    private case Empty()
    // () => è un supplier, quando la si userà allora si genererà l'errore
    // toccarlo è ()
    // usando il supplier allora cambio la struttura
    // non prendo più oggetti ma funzioni (supplier)
    // usare call-by-need per avere il valore fisso una volta chiamata la prima volta
    // si deve usare lazy val nel corpo della funzione per avere gli stessi valori
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def head[A](s: Stream[A]): A =
      s match
        case Cons(h, _) => h()

    def tail[A](s: Stream[A]): Stream[A] =
      s match
        case Cons(_, t) => t()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    // questa potremme inloopparsi, serve limitare fino a dove (simile a boxed o limit di java)
    // in questo caso Nil
    def toList[A](stream: Stream[A]): Sequence[A] = stream match
      case Cons(h, t) => Sequence.Cons(h(), toList(t()))
      case _ => Sequence.Nil()

    // usiamo cons con c minuscolo per che vogliamo una funzione
    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    // sembra che sia infinita senza caso base
    // nel caso call-by-need non è infinito ma fino a quando serve
    // ha fine nel momento in cui non si usa
    // con le cose call-by-need utili per avere generatori infiniti di funzioni(qualsiasi trasformazioni o oggetti)
    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

  end Stream

@main def tryStreams =
  import Streams.*

  val str1 = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
  val str2 = Stream.map(str1)(_ + 1) // {1,2,3,4,..}
  val str3 = Stream.filter(str2)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
  val str4 = Stream.take(str3)(10) // {1,2,21,22,..,28}
  println(Stream.toList(str4)) // [1,2,21,22,..,28]

  lazy val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]

  // non è ben capibile cosa succede prima o poi di qualcosa
  def random(): Double = Math.random()
  val iter = Stream.iterate(random())(_ + 1)
  val s = Stream.map(iter)(_ + 1)
  // trucco sintattico, usando il carry con extend
  // extension (n: Int) def sum(m: Int): Int = n + m
  // in questo modo si antepone il primo parametro
  // trucco sintattico per fare la chiamata (NON e' OO)
  // in questo modo si puo chiamare la sum (3)sum(4) (valida anche la sintassi di prima)
  // anche senza parentesi per andare più in linea 3 sum 4
  // oppure si può scrivere la posto della sum +++, quindi 3 +++ 4
