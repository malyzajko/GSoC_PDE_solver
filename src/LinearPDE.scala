/**
 * Linear PDEs are of the form:
 * Lu = alpha u_{tt} + beta u_{xt} + gamma u_{xx} + f = 0
 * where a, b, c and f are functions
 * 
 * To solve, we need boundary condition V, step size and a 
 * For the while, I've assumed that the steps are equally sized in x and t
 * 
 * u_{xx} + u_{tt} = c(x, t)
 * 
 * a: (Double, Double) => Double,
                 b: (Double, Double) => Double, 
                 c: (Double, Double) => Double,
                 f: (Double, Double) => Double, 
 */

class LinearPDE (V: Boundary, c: (Double, Double) => Double)
{
  def generateSolution (step: Double) : Array[Array[Double]] = {
    
    val xsize = (((V.xbint._2 -V.xbint._1)/step abs) round).toInt+1
    val tsize = (((V.tbint._2 -V.tbint._1)/step abs) round).toInt+1
	
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
	for (i <- 0 until tsize){
	  u(0)(i) = V.b3._1 (xmin, tmin+i*step)
	  u(xsize-1)(i) = V.b4._1 (xmax, tmin+i*step)
	}
	
    def generateSolution1 {
      
	  def u00 (i: Int, j: Int) = {
	    (u(i-1)(j) + u(i+1)(j) + u(i)(j-1) + u(i)(j+1) -
             c(xmin+i*step,tmin+j*step)*step*step )/4.0
	  }
      var diff = 0.0 //Difference between iterations
      
      for(i <- 1 until xsize-1){
	    for(j <- 1 until tsize-1){
	      v(i)(j) = u00(i, j)
	      diff += (v(i)(j) - u(i)(j)).abs
	    }
	  }
	  
	  for(i <- 1 until xsize-1)
	    for(j <- 1 until tsize-1){
	  	  u(i)(j) = v(i)(j)
	    }
	  
	  if (diff > 0.001) generateSolution1
	   
	}
   
	generateSolution1  
    u
  }
}


object LinearPDE
{
  
  def main (args: Array[String]){
    
    import scala.math._
    /*
     * testing with the Laplace equation : a u_{xx} + u_{tt} = 0 
     * and a boundary with constant potential along all sides except for
     * at the bottom
     */
    val bot = (x: Double, t: Double) => x*x*x + t*t*t + 2.0*x*x*t + 3.0*x*t*t + 4.0*x + 5.0*t +6.0
    val top = (x: Double, t : Double) => sin(Math.Pi*x)*exp(-Math.Pi)
    val sides = (x: Double, t:Double ) => 0.0
    val testBoundary = new Boundary(
      (bot, (-1, 1), (1, -1)),
      (bot, (-1, 1), (1, 1)),
      (bot, (-1, 1), (0, -1)),
      (bot, (-1, 1), (0, 1))
    )
    def solution (x: Double, t: Double) = sin(Math.Pi*x)*exp(-Math.Pi*t)
    val test = new LinearPDE(testBoundary, (x, t) => 12*x + 10* t)
    val result = test.generateSolution(0.2)
    for (i <- 0 until result.size){
      for (j <- 0 until result(1).size) {
        printf(" %2.2f ",result(i)(j))
      }
      println("")
    }
  }
}











