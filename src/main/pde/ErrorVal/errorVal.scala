//TBD
package pde.model.errorData;
import pde.model.expression.{BFunction}
import pde.model.{DirichiletBoundary}
  import ceres.smartfloat.SmartFloat
  import pde.model.{PDE}

  class OutOfBoundsException extends Exception

  class ErrorData(private val data: Array[Array[SmartFloat]], private val pde: PDE,
                  boundary: DirichiletBoundary, private val xstep: Double, private val tstep: Double){

    private val f = boundary.f
    private val xmin = boundary.xmin
    private val xmax = boundary.xmax
    private val tmin = boundary.tmin
    private val tmax = boundary.tmax

    def apply(x: Double, t: Double): SmartFloat = {
      val xindex = (x-xmin)/xstep
      val tindex = (t-tmin)/tstep
      if (x < xmin || x > xmax || t < tmin || t > tmax) throw new OutOfBoundsException
      else if (xindex % 1.0 == 0.0 && tindex % 1.0 == 0.0) data(xindex.toInt)(tindex.toInt)
      else getData(x, t)
    }
    private def getData(x: Double, t: Double) : SmartFloat = {
      val xindex = (xmin + x)/xstep
      val tindex = (tmin + t)/tstep
      val xInt = xindex.toInt
      val tInt = tindex.toInt
      if (xindex % 1.0 == 0.0) {
        val points = Array[SmartFloat](data(xInt)(tInt),
          data(xInt+1)(tInt))
        val weights = Array[Double](xInt + 1 - xindex, xindex - xInt)
        val sumWeights = weights(0) + weights(1)
        (points(0) * weights(0) + points(1) * weights(1))/sumWeights
      }
      else if (tindex % 1.0 == 0.0){
        val points = Array[SmartFloat](data(xInt)(tInt),
          data(xInt)(tInt))
        val weights = Array[Double](tInt + 1 - tindex, tindex - tInt)
        val sumWeights = weights(0) + weights(1)
        (points(0) * weights(0) + points(1) * weights(1))/sumWeights
      }
      else {
        def dist(x: Double, y: Double)(x1: Double, y1: Double) : Double = {
          math.sqrt((x - x1) * (x - x1) + (y - y1) * (y - y1))
        }
        val points = Array[SmartFloat](data(xInt)(tInt),
          data(xInt+1)(tInt),
          data(xInt)(tInt+1),
          data(xInt+1)(tInt+1)
        )
        val weights: Array[Double]= {
          val temp = Array[(Double, Double)]((xindex - xInt, tindex - tInt),
            (xInt + 1 - xindex, tindex - tInt),
            (xindex - xInt, tInt + 1 - tindex),
            (xInt + 1 - xindex, tInt + 1 - tindex))

          val corners = Array((0, 0), (1, 0), (0, 1), (1, 1))
            (for(i <- 0 until 4)
            yield dist(temp(i)._1, temp(i)._2)(corners(i)._1, corners(i)._2)).toArray
        }
        def inverseDistances() = {
          val sum = weights.foldLeft(0.0)((x, y) => x + y)
          for(i <- 0 until 4)
            weights(i) = weights(i)/sum
          for(i <- 0 until 4)
            weights(i) = 1/weights(i)
        }
          inverseDistances()
        val numerator = {
          val temp = for((x, w) <- points zip weights)
          yield x * w
            temp.foldLeft(SmartFloat(0.0))((x, y) => x + y )
        }
        val denominator = weights.foldLeft(0.0)((x, y) => x + y)
        numerator/denominator
      }
    }

    override def toString(): String = {
      "Solution to: " + pde
    }
  }
