/**
 * File: Boundary.scala
 * 
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
 *      |								 |
 *      ----------------------------------
 *      xbot  
 */

class Boundary (
    val b1: ((Double, Double) => Double, (Double, Double), (Int, Double)),
    val b2: ((Double, Double) => Double, (Double, Double), (Int, Double)), 
    val b3:  ((Double, Double) => Double, (Double, Double), (Int, Double)),
    val b4:  ((Double, Double) => Double, (Double, Double), (Int, Double))
    ) {
  
  val xbint = b1 match{
    case (_, (a, b), _) => (a, b)
    case _ => throw new Exception ("malformed boundary")
  }
  
  val tbint = b3 match{
    case (_, (a, b), _) => (a, b)
    case _ => throw new Exception ("malformed boundary")
  }
  
	
}