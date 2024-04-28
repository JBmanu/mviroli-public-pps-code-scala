package u07.traits

abstract class Parser[T]:
  def parse(t: T): Boolean  // accept the token! did it work?
  def end: Boolean        // tokens are over! is this ok?
  def parseAll(seq: Seq[T]): Boolean = (seq forall (parse(_))) & end // could use & or &&

// A parser consuming any sequence of elements in `chars`
case class BasicParser(chars: Set[Char]) extends Parser[Char]:
  override def parse(t: Char) = chars contains t
  override def end = true

// A mixin rejecting empty strings (see: a sort of decorator!)
trait NonEmpty[T] extends Parser[T]:
  private var empty = true
  abstract override def parse(t: T) = {empty = false; super.parse(t)} // who is super??
  abstract override def end = !empty && super.end

class NonEmptyBasicParser(chars: Set[Char]) extends BasicParser(chars) with NonEmpty[Char]

object TryParsers extends App:
  println(BasicParser(Set('a','b','c')).parseAll("aabc".toList)) // true
  println(BasicParser(Set('a','b','c')).parseAll("aabcdc".toList)) // false
  println(BasicParser(Set('a','b','c')).parseAll("".toList)) // true

  // Note NonEmpty being "stacked" on to a concrete class
  // Bottom-up decorations: NonEmptyParser -> NonEmpty -> BasicParser -> Parser
  println(NonEmptyBasicParser(Set('0','1')).parseAll("0101".toList)) // true
  println(NonEmptyBasicParser(Set('0','1')).parseAll("0123".toList)) // false
  println(NonEmptyBasicParser(Set('0','1')).parseAll(List())) // false


