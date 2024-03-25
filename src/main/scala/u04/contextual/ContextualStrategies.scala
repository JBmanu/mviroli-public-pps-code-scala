package u04.contextual
import u03.extensionmethods.Sequences.* 
import u03.extensionmethods.Optionals.* 
import Sequence.*

object ContextualStrategies:

  /* a standard max over a sequence of Int, passing a strategy for ordering 
   * seq as a "context". 
   * max could be made generic, not developed here...
   */
  def max(seq: Sequence[Int])(using ordering: (Int, Int) => Boolean): Int = seq match
    case Cons(h1, Cons(h2, t)) => max(Cons(if ordering(h1, h2) then h1 else h2, t))
    case Cons(h1, Nil()) => h1
      
  @main def tryContextualStrategies() =
    // context is passed with specific syntax
    println:
      max(Cons(10, Cons(30, Cons(20, Nil()))))(using _ > _)
    println:
      max(Cons(10, Cons(30, Cons(20, Nil()))))(using _ < _)

    // the given defined here needs not be imported
    // name of given is not very helpful, it can be avoided  
    given ((Int, Int) => Boolean) = _ > _

    println:
      max(Cons(10, Cons(30, Cons(20, Nil()))))
