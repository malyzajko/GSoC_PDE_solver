//TBD
/**
  * File: Boundary.scala
  * Here is a basic data class to define boundary conditions.
  * For the while it's only for 4 sided boundaries.
  * The boundaries are defined as (g(x), (min, max), (x, double))
  * The last tuple is to determine which variable is held fixed and at what point.
  * It's zero based, so x is 0 and t is 1.
  *
  * Example
  * (g(x), (0, 1), (1, 3)) means:
  * u(x, 3) = g(x) for 0 <= x <= 1
  *
  * boundaries are assumed to be in order of xbot, xtop, tbot, ttop
  * For the while the boundaries are also rectangular, but I've set it for
  * intervals for expanding on it later
  *
  *      _________________________________ xtop
  *      |                                |
  *      |                                |
  *      |tleft                           | tright
  *      |                                |
  *      ----------------------------------
  *      xbot
  *
  *      curerntly only has rectangular coords
  */

package pde.boundary;
import pde.expression.{BFunction}
sealed abstract class Boundary

class RectBoundary(
  val b1: BFunction,
  val b2: BFunction,
  val b3: BFunction,
  val b4: BFunction
) extends Boundary {
  assert(assertBoundary)

  private def assertBoundary = {
    def approxEqual(a: Double, b: Double) : Boolean = {
      if ( (a-b).abs < 0.001 ) true else false
    }
    def compareBFs(f: BFunction, g: BFunction) : Boolean = {
      if (f.u.fixed == g.u.fixed) {
        f.interval == g.interval
      }
      else {
        if (f.lowerPoint == g.lowerPoint)      {approxEqual(f.lowerValue, g.lowerValue)}
        else if (f.lowerPoint == g.upperPoint) {approxEqual(f.lowerValue, g.upperValue)}
        else if (f.upperPoint == g.lowerPoint) {approxEqual(f.upperValue, g.lowerValue)}
        else if (f.upperPoint == g.upperPoint) {approxEqual(f.upperValue, g.upperValue)}
        else {false}
      }

    }
    val points = Set(b1.lowerPoint, b1.upperPoint,
      b2.lowerPoint, b2.upperPoint,
      b3.lowerPoint, b3.upperPoint,
      b4.lowerPoint, b4.upperPoint)

    val pointComparisons = for {boundary1 <- List(b1, b2, b3, b4)
      boundary2 <- List(b2, b3, b4)}
    if (!compareBFs(boundary1, boundary2))
      { false }

    (points.size==4)

  }


}
