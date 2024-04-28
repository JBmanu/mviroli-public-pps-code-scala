package u06.structure

trait Counter:
  def increment(): Unit
  def getValue(): Int

object Counter:
  def apply(): Counter = new CounterImpl

class CounterImpl private[structure] () extends Counter:
  var count = 0
  override def increment(): Unit = count += 1
  override def getValue(): Int = count

object TryCounters extends App:
  val c = Counter()
  println(c) // u05.structure.CounterImpl@4411d970
