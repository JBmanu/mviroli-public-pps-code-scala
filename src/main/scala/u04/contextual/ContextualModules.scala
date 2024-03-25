package u04.contextual

import u03.extensionmethods.Sequences.*
import u03.extensionmethods.Optionals.*
import Sequence.*

object ContextualModules:
  
  trait IntOrderingModuleType:
    def greater(t1: Int, t2: Int): Boolean

  def max(seq: Sequence[Int])(using ordering: IntOrderingModuleType): Int = seq match
    case Cons(h1, Cons(h2, t)) => max(Cons(if ordering.greater(h1, h2) then h1 else h2, t))
    case Cons(h1, Nil()) => h1

  // defining a module implementing the contract
  object MyIntOrderingModule extends IntOrderingModuleType:
    def greater(t1: Int, t2: Int): Boolean = t1 > t2

  // defining a canonical term taking an existing module
  given iom: IntOrderingModuleType = MyIntOrderingModule

@main def tryContextualModules =  
  import ContextualModules.* 
  // importing all given
  import ContextualModules.given 
  // importing a specific given by: import ContextualModules.iom
  // importing just * would not work!

  println:
    max(Cons(10, Cons(30, Cons(20, Nil()))))

  val ord = summon[IntOrderingModuleType] // gives ContextualModules.iom
