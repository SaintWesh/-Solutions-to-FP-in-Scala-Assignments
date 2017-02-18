package calculator

import scala.math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal((b() * b()) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal {
      computeDelta(a, b, c)() match {
        case x:Double if x > 0 => Set((-b() - sqrt(x)) / (2 * a()), (-b() + sqrt(x)) / (2 * a()))
        case x:Double if x == 0 => Set(-b() / (2 * a()))
        case _ => Set()
      }
    }
  }
}
