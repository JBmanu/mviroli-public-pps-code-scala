package u07.exports

object MyStandardExports:
  export scala.collection.mutable.{
          ListBuffer => StandardList,
          HashSet => StandardSet,
          HashMap => StandardMap
  }
  export scala.Array.*
  export java.lang.Math.*
  export java.util.Date

@main def tryExports =
  import MyStandardExports.*

  println(StandardSet(10,20,30).getClass) // using mutable.HashSet alias
  println(sin(PI / 2)) // using Math.*
  println(concat(Array(10, 20), Array(30, 40)).toList) // using Array.*
  println(new Date()) // using java.util.Date



