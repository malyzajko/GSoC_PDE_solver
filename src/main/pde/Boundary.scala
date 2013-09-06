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
import pde.expression.{BFunction, Condition}
sealed abstract class Boundary

class invalidCondition extends Exception

class RectBoundary(
  b1: BFunction,
  b2: BFunction,
  b3: BFunction,
  b4: BFunction
) extends Boundary {
  assert(assertBoundary)

  private def assertBoundary = {
    def approxEqual(a: Double, b: Double) : Boolean = {
      if ( (a-b).abs < 0.001 ) true else false
    }

    def compareBFs(f: BFunction, g: BFunction) : Boolean = {
      if (f.u.c == g.u.c) {
        f.interval == g.interval
      }
      else {
        if (f.lowerPoint == g.lowerPoint)      f.lowerValue == g.lowerValue
        else if (f.lowerPoint == g.upperPoint) f.lowerValue == g.upperValue
        else if (f.upperPoint == g.lowerPoint) f.upperValue == g.lowerValue
        else if (f.upperPoint == g.upperPoint) f.upperValue == g.upperValue
        else false
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

  val f = b1.u.function

  val (bottom: BFunction, top: BFunction, left: BFunction, right: BFunction) = {
    var sorted = (scala.collection.mutable.ArrayBuffer(b1, b2, b3, b4)).sortWith(
      (a, b) => a.lowerPoint(f.t) < b.lowerPoint(f.t))
    val bot = sorted.find ( b => f.t == b.u.fixed ) match {
      case Some(b) => b
      case _ => throw new Exception
    }
    val tp = sorted.last
    sorted -= (bot, tp)
    sorted = sorted.sortWith(
      (a, b) => a.lowerPoint(f.x) < b.lowerPoint(f.x))
    val lef = sorted.head
    val rig = sorted.last
    (bot, tp, lef, rig)
  }


}

class ThreeSidedBoundary(
  val bottom: BFunction,
  val b2: Condition,
  val b3: Condition
) extends Boundary {
  assert(assertBoundary)

  private def assertBoundary = {

    if (b2.c == bottom.interval.lo && b3.c == bottom.interval.hi)
      b2.exp.eval(bottom.lowerPoint.coord) == bottom.lowerValue &&
    b3.exp.eval(bottom.upperPoint.coord) == bottom.upperValue
    else if (b2.c == bottom.interval.hi && b3.c == bottom.interval.lo)
      b2.exp.eval(bottom.upperPoint.coord) == bottom.upperValue &&
    b3.exp.eval(bottom.lowerPoint.coord) == bottom.lowerValue
    else false
  }

  val f = bottom.u.function

  val (left, right)= {
    if (b2.c < b3.c) (b2, b3)
    else if (b2.c > b3.c) (b3, b2)
    else throw new invalidCondition
  }


}
