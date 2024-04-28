package u05

object Fancy extends App:

  case class Person(name: String, surname: String, year: Int)

  def makePerson(name: String, surname: String = "?", year: Int = 1900) = Person(name, surname, year)

  println(makePerson("bob", "hope")) // skipping args with a default
  println(makePerson(name = "bob", surname = "hope")) // using named args
  println(makePerson(surname = "hope", name = "bob")) // no order
  val p = makePerson(name = "bob", surname = "hope", year = 1920)
  println(p)
  println(p.copy(year = 1921)) // copy method of case classes
  println(makePerson(name = "bob", "hope")) // some name
  println(makePerson("bob"))
  println{makePerson{"bob"}} // curly braces

  @annotation.tailrec // call by-name
  def mywhile(condition: => Boolean)(body: =>Unit): Unit =
    if condition then {body; mywhile(condition)(body)}

  var n = 5
  var res = 1
  mywhile( n>=1 ) { // the same as while!!
    res = res * n
    n = n - 1
  }
  
  println("factorial "+res)
