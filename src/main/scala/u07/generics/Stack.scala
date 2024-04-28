package u07.generics

trait Stack[T]:
  def top: Option[T]
  def pop: Option[(T,Stack[T])]
  def push(t: T): Stack[T]
  def toList: List[T]

object Stack:

  def apply[T](elems: T*): Stack[T] = StackImpl(elems.toList)

  private class StackImpl[T](val list: List[T]) extends Stack[T]:
    override def top = list.headOption
    override def pop = top map ((_,StackImpl(list.tail)))
    override def push(t: T) = StackImpl(t :: list)
    override def toList = list

object TryStacks extends App:
  var stack = Stack(10)
  stack = stack push 20
  println(stack.top) // Some(20)
  stack = stack.pop.get._2 //[10]
  println(stack.top) // Some(10)
  stack = stack.pop.get._2 //[]
  println(stack.top) // None

  // COVARIANCE PROBLEM
  //var generalStack: Stack[Any] = Stack[Int](10,20,30)
  //generalStack = generalStack.push("a string") // this would corrupt the stack
