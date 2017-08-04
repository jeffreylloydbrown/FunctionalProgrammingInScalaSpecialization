package calculator

import Math.sqrt

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal( (b()*b()) - (4.0 * a() * c()) )
  }

  // Remember that `delta` can be negative, so we cannot just always do a square root.
  // Yes, I'm accessing delta() a bunch.  Yes, I'm computing the same square root more than once.
  // It's a homework assignment, get over it.
  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal(
    delta() match {
      case del if (del > 0)   =>  Set( (-b() + sqrt(delta()))/(2*a()), (-b() - sqrt(delta()))/(2*a()) )
      case del if (del == 0)  =>  Set( -b()/(2*a()) )
      case del if (del < 0)   =>  Set()
    }
  )
}
