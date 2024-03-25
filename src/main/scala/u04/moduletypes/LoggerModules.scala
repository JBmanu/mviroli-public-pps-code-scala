package u04.moduletypes

object LoggerModules:

  // type for modules exposing definition of a log method (no implementation)
  trait LoggerModuleType:
    def log(s: String): Unit

  // implementation of a module adhering to LoggerModuleType
  object BasicLoggerModule extends LoggerModuleType:
    def log(s: String): Unit = println(s)

  // implementation of a module adhering to LoggerModuleType
  object EmptyLoggerModule extends LoggerModuleType:
    def log(s: String): Unit = {}

  // method call requiring indication of a LoggerModuleType
  def loggedCall[A, B](name: String, f: A => B, a: A)(LoggerModule: LoggerModuleType): B =
    LoggerModule.log(s"calling function $name with input $a")  
    f(a)

@main def tryLoggers =
  import LoggerModules.* 

  // passing the desired module
  println(loggedCall("sin", Math.sin(_), 0.5)(BasicLoggerModule))
  println(loggedCall("sin", Math.sin(_), 0.5)(EmptyLoggerModule))
