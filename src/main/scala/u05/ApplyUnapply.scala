package u05

object ApplyUnapply extends App:

  // another, important application: factories hiding implementation
  trait Person:
    def name: String
    def surname: String

  object Person: // the companion object of trait/class Person
    def apply(name: String, surname: String): Person =
      PersonImpl(name,surname)
    def unapply(p: Person): Option[(String, String)] =
      Some(p.name, p.surname) 
    private class PersonImpl(override val name: String,
                             override val surname:String) extends Person:
      assert(name != null && surname != null)

  // Showcasing apply
  val p: Person = Person("mario", "rossi")  // Hiding PersonImpl
  println(s"$p, $p.name, $p.surname")
  // Showcasing unapply
  println(p match {case Person(n, s) => n+s}) // mariorossi
  println(Person.unapply(p).map((n, s) => n+s).get) // roughly equivalent formulation
  // Use of matching/unapply in val definition
  val Person(n, s) = p // add `: @unchecked` to avoid warning of possible match error
  println(n + s)
