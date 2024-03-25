package u04.contextual

@main def tryContextualMathModule =
  import u04.MathModules.*

  // probability of having x successes over n trials, where each success has prob. p
  // abstracting from specific implementation of the math module
  def binomialProbability(n: Int, x: Int, p: Double)(using mm: MathModuleType): Double =
    mm.factorial(n) / mm.factorial(x) / mm.factorial(n-x) * 
    mm.exp(p, x) * mm.exp(1-p, n-x)

  // injecting the depenency by a given!!
  given MathModuleType = ProductionMathModule  

  // probability of 6 heads on 10 coin tosses
  println:
    binomialProbability(10, 6, 0.5)
  println:
    binomialProbability(10, 6, 0.5)
  

