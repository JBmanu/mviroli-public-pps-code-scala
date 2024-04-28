package u05

// A person with immutable name/surname
// mutable married flag, and toString
trait Person:
  def name: String
  def surname: String
  def married: Boolean
  def married_=(state: Boolean): Unit
  // a template method
  override def toString(): String = s"$name $surname $married"

// easiest implementation
// with overriding class parameters becoming fields
class PersonImpl1(override val name: String,
                  override val surname: String,
                  override var married: Boolean) extends Person

// surname as a constant, name with overridden getter
class PersonImpl2(_name: String,
                  override var married: Boolean) extends Person:
  override def name =
    require(_name != null) // may throw IllegalArgumentException
    _name
  override val surname = "?"

// overridden getters and setters of married
class PersonImpl3(override val name: String,
                  override val surname: String,
                  private var _married: Boolean) extends Person:
  override def married =
    println("getting married property")
    _married
  override def married_=(m: Boolean) =
    require (_married != true || m != false)
    _married = m

// Note uniform access
@main def usePerson() =
  val p: Person = PersonImpl1("mario", "rossi", false)
  println(p)
  val p2: Person = PersonImpl2("mario", false)
  println(p2)
  val p3: Person = PersonImpl3("mario", "rossi", false)
  println(p3)
  p3.married = true
  println(p3)
