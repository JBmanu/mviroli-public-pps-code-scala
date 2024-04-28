package u07.extensions

object OOPFPLists extends App:

  enum MyList[+A]:
    case Cons(h: A, t: MyList[A])
    case Empty

  object MyList:
    import MyList.*
    def apply[A](a: A*) = a.foldRight[MyList[A]](Empty)(Cons(_, _))
    extension [A](list: MyList[A])
      def foldRight[B](init: =>B)(op: (A, B) => B): B = list match
        case Empty => init
        case Cons(h, t) => op(h, t.foldRight(init)(op))
      def foldLeft[B](init: =>B)(op: (B, A) => B): B = list match
        case Empty => init
        case Cons(h, t) => t.foldLeft(op(init, h))(op)
      def map[B](f: A => B): MyList[B] =
        list.foldRight[MyList[B]](Empty)((a, b) => Cons(f(a), b))
      def filter(p: A => Boolean) =
        list.foldRight[MyList[A]](Empty)((a, b) => if p(a) then Cons(a, b) else b)
      def reduce(op: (A, A) => A): A = list match
        case Cons(h, t) => t.foldLeft(h)(op)

  import MyList.*
  val l = MyList(10, 20, 30, 40, 50)
  println(l map (_ - 1) filter (_ < 30) reduce (_ + _)) //57