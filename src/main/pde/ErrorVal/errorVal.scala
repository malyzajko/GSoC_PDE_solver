package pde.erroVal
import pde.expression._
import pde.boundary._
import pde._

class OutOfBoundsException extends Exception

class errorData(val data: Array[Double], val error: Array[Double], boundary: RectBoundary){

  private val f = boundary.b1.u.function
  private val (bottom: BFunction, top: BFunction, left: BFunction, right: BFunction) = {
    var sorted = (scala.collection.mutable.ArrayBuffer
      (boundary.b1, boundary.b2, boundary.b3, boundary.b4)).sortWith(
      (a, b) => a.lowerPoint(f.t) < b.lowerPoint(f.t))
    val bot = sorted.find ( b => f.t == b.fixed.variable ) match {
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

  private val xmin = bottom.interval.a
  private val xmax = bottom.interval.b
  private val tmin = left.interval.a
  private val tmax = left.interval.b

  def apply(x: Double, t: Double) = {
    if (x < xmin || x > xmax || t < tmin || t > tmax) throw new OutOfBoundsException

    
  }
  
}

