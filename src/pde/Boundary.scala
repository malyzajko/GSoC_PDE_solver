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
 *      
 *      curerntly only has rectangular coords
 */

package pde
import pde.variable._
import pde.expression._

class RectBoundary (
    val b1: BFunction,
    val b2: BFunction, 
    val b3: BFunction,
    val b4: BFunction
    ) {
	assert(assertBoundary)
	
	private def assertBoundary = {
	  import scala.collection.immutable.Map
	  def compareBFs(f: BFunction, y: BFunction) = {
	    if (f.lowerPoint == y.lowerPoint) f.lowerValue == y.lowerValue
	    else if (f.lowerPoint == y.upperPoint) f.lowerValue == y.upperValue
	    else if (f.upperPoint == y.lowerPoint) f.upperValue == y.lowerValue
	    else if (f.upperPoint == y.upperPoint) f.upperValue == y.upperValue
	  }
	  val points = Set(b1.lowerPoint, b1.upperPoint, 
	                   b2.lowerPoint, b2.upperPoint, 
	                   b3.lowerPoint, b3.upperPoint,
	                   b4.lowerPoint, b4.upperPoint)
	  
	  val pointComparisons = for {boundary <- List(b1, b2, b3, b4)
	       						  boundary2 <- List(b2, b3, b4)}
	      						yield compareBFs(boundary, boundary2)
	      						
	  (points.size==4) && (!pointComparisons.contains(false))
			
	}
  
	
}