package u05.structure

package p1:
  package p2:
    object O:
      val v = 1

object Import extends App:

  println(u05.structure.p1.p2.O.v) // full qualification
  println(p1.p2.O.v)    // relative qualification

  import p1.*       // importing u05.structure.p1
  println(p2.O.v)

  import p2.*       // the same as: import u05.structure.p1.p2.*
  println(O.v)

  import u05.structure.p1.p2.O.* // static import of O's props
  println(v)

  import u05.structure.p1.p2.{O => TheObject} // alias
  println(TheObject.v)

  import scala.collection.mutable.{ Seq => MutableSeq } // renaming Seq
  println(s"${Seq(10,20,30,40)}, ${MutableSeq(10,20,30,40)}")