package prove

object Sequences:

  enum Sequence[A]:
    case Nil()
    case Cons(head: A, tail: Sequence[A]) // questa ricorsività ci permette di avere una lista variabile

  object Sequence:
    def isEmpty[A](s: Sequence[A]): Boolean =
      s match
        case Nil() => true
        case _ => false

    // somma fatta tramite ricorsione perche è l'unico modo per iterare
    // ricorsione non tail, siamo nello stack
    // la piu semplice me la tengo
    def sum(s: Sequence[Int]): Int =
      s match
        case Nil() => 0
        case Cons(h, t) => h + sum(t)

    def map[A, B](s: Sequence[A], f: A => B): Sequence[B] =
      s match
        case Nil() => Nil()
        case Cons(h, t) => Cons(f(h), map(t, f))

    // case sono a cascata, passato uno sono gia nell'altro caso
    //dopo if ce l'else senza scriverlo

    def filter[A](s: Sequence[A], f: A => Boolean): Sequence[A] =
      s match
        case Cons(h, t) if f(h) => Cons(h, filter(t, f))
        case Cons(h, t) => filter(t, f)
        case Nil() => Nil()
