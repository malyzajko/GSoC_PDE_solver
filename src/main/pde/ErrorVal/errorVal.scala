//TBD
package pde.model.errorData;
import pde.model.expression.{BFunction}
import pde.model.{DirichiletBoundary}
import ceres.smartfloat.SmartFloat

class OutOfBoundsException extends Exception

class ErrorData(private val data: Array[Array[SmartFloat]], boundary: DirichiletBoundary){
  
  private val f = boundary.f
  private val xmin = boundary.xmin
  private val xmax = boundary.xmax
  private val tmin = boundary.tmin
  private val tmax = boundary.tmax

  def apply(x: Double, t: Double): SmartFloat = {
    if (x < xmin || x > xmax || t < tmin || t > tmax) throw new OutOfBoundsException
    else if (x % 1.0 == 0.0 && t % 1.0 == 0.0) data(x.toInt)(t.toInt)
    else getData(x, t)
  }
  private def getData(x: Double, t: Double) = {
    val points = Array[SmartFloat](data(x.toInt)(t.toInt),
                                   data(x.toInt+1)(t.toInt),
                                   data(x.toInt)(t.toInt+1),
                                   data(x.toInt+1)(t.toInt+1)
                                 )
    val avg = (points.foldLeft(SmartFloat(0.0))(_ + _))/4
    val stdSquared = (points.foldLeft(SmartFloat(0.0))( (x, y) => x + SmartFloat.pow(y-avg, 2)))/3
    val std = SmartFloat.sqrt(stdSquared)

    avg addError std
  }
}
