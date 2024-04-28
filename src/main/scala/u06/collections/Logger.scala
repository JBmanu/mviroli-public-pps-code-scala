package u06.collections

// a mutable class, using a mutable val
class Logger:
  private val strings = collection.mutable.ListBuffer[String]()
  def log(string: String): Unit = strings.addOne(string)
  def getLog: Iterable[String] = strings.view // a lazy, immutable wrapper

// a mutable class, using an immutable var
class Logger2:
  private var strings = Seq[String]() // List
  def log(string: String): Unit = strings = strings :+ string
  def getLog: Iterable[String] = strings

// an immutable class, using an immutable val
class Logger3 private (private val strings: List[String]):
  def this() = this(List[String]())
  def log(string: String): Logger3 = Logger3(strings :+ string)
  def getLog: Iterable[String] = strings.view // a lazy, immutable wrapper

object TryLogger extends App : // which one is better??
  val logger = new Logger()
  logger.log("str1")
  logger.log("str2")
  logger.log("str3")
  logger.getLog foreach (println(_))

  var logger3 = new Logger3()
  logger3 = logger3.log("str1")
  logger3 = logger3.log("str2")
  logger3 = logger3.log("str3")
  logger3.getLog foreach (println(_))
