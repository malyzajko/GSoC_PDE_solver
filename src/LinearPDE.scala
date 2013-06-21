/**
 * Linear PDEs are of the form:
 * Lu = au_{tt} + bu_{xt} + cu_{xx} + f = 0
 * where a, b, c and f are functions
 * 
 * To solve, we need boundary condition V, step size and a constant.
 * For the while, I've assumed that the steps are equally sized in x and t
 * 
 * a: (Double, Double) => Double,
                 b: (Double, Double) => Double, 
                 c: (Double, Double) => Double,
                 f: (Double, Double) => Double, 
 */

class LinearPDE (V: Boundary, step: Double, c: (Double, Double) => Double)
{
  def generateSolution : Array[Array[Double]] = {
    val xsize = (((V.xbint._2 -V.xbint._1)/step abs) round).toInt
    val tsize = (((V.tbint._2 -V.tbint._1)/step abs) round).toInt

    //Array to store solution points. u(x)(t)
    var u = Array.fill(xsize, tsize)(0.0)
    var v = Array.fill(xsize, tsize)(0.0)

    //Fill boundaries
    val tmin = V.b1._2._1
    val tmax = V.b2._2._2
    val xmin = V.b3._2._1
    val xmax = V.b4._2._2
    for (i <- 0 until xsize){
      u(i)(0) = V.b1._1 (xmin+i*step, tmin)
      u(i)(tsize-1) = V.b2._1 (xmin+i*step, tmax)
    }
    println("test:"+u(0)(0))
    
    for (i <- 0 until tsize){
      u(0)(i) = V.b3._1 (xmin, tmin+i*step)
      u(xsize-1)(i) = V.b4._1 (xmax, tmin+i*step)
    }
    
    for(i <- 1 until xsize-1){
      for(j <- 1 until tsize-1){
        v(i)(j) = u(i-1)(j) + u(i+1)(j) +  u(i)(j-1) + u(i)(j+1) - c(xmin+i*step, tmin+i*step)
      }
    }
    for(i <- 1 until xsize-1)
      for(j <- 1 until tsize-1){
    	  u(i)(j) = v(i)(j)
      }
    u
  }
}


object LinearPDE
{
  
  def main (args: Array[String]){
    /*
     * testing with  u_{xx} + u_{tt} -12x-10t  = 0
     * and boundary: u(x, t) = x^3 + t^3 + 2x^2t+ 3xt^2+ 4x+ 5t + 6
     */
    val testBF = (x: Double, t: Double) => x*x*x + t*t*t + 2*x*x*t + 3*x*t*t + 4*x + 5*t + 6
    val testBoundary = new Boundary(
      (testBF, (-1, 1), (0, -1)),
      (testBF, (-1, 1), (0, 1)),
      (testBF, (-1, 1), (1, -1)),
      (testBF, (-1, 1), (1, 1))
    )
    val test = new LinearPDE(testBoundary, 0.2, (x, t) => 12*x + 10*t)
    val result = test.generateSolution
    for (i <- 0 until result.size)
      for (j <- 0 until result(1).size) {
        println("result("+i+")("+j+")="+result(i)(j))
      }
  }
}











