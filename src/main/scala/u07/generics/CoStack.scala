package u07.generics

trait CoStack[+T]: // a covariant stack
  def top: Option[T]
  def pop: Option[(T,CoStack[T])]
  def push[R >: T](r: R): CoStack[R]  // a more general push is needed
  def toList: List[T]

// same implementation of Stack
object CoStack:

  def apply[T](elems: T*): CoStack[T] = CoStackImpl(elems.toList)

  private case class CoStackImpl[+T](list: List[T]) extends CoStack[T]:
    override def top = list.headOption
    override def pop = top map ((_, CoStackImpl(list.tail)))
    override def push[R >: T](r: R) = CoStackImpl(r :: list)
    override def toList = list


object TryCoStacks extends App:
  var stack: CoStack[Any] = CoStack[Int](10, 20, 30)
  stack = stack push "a string"
  val list: List[Any] = stack.toList
  println(list)