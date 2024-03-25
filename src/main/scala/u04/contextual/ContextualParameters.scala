package u04.contextual
import u03.extensionmethods.Sequences.* 
import u03.extensionmethods.Optionals.* 
import Sequence.*

object ContextualParameters:

  /* A standard max over a sequence, with an additional "configuration"
   * or "contextual" parameter, specified by using clause. Note the using 
   * clause is not necessary in the recusive call, since the context can be
   * considered the same.
   */
  def max(seq: Sequence[Int])(using orElse: Int): Int = seq match
    case Cons(h1, Cons(h2, t)) => max(Cons(if h1 > h2 then h1 else h2, t))
    case Cons(h1, Nil())       => h1
    case _                     => orElse

  given standardDefault: Int = -1 // if using an Int is needed, suggest to take this!  
      
@main def tryContextualParameters() =
  import ContextualParameters.* 

  // input and context are passed with specific syntax
  println:
    max(Cons(10, Cons(30, Cons(20, Nil()))))(using -1)
  println:
    max(Nil())(using -1)

  import ContextualParameters.given // or ContextualParameters.standardDefault

  println:
    max(Cons(10, Cons(30, Cons(20, Nil())))) // no need for the using clause