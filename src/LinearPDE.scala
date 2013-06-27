/**
 * Linear PDEs are of the form:
 * Lu = alpha u_{tt} + beta u_{xt} + gamma u_{xx} + d u_t + e u_x + f u + g(x, t) = 0
 * 		a              b             c
 * where a, b, c, d, g and f are functions of x and t
 * 
 * To solve, we need boundary condition V, step size and a 
 * For the while, I've assumed that the steps are equally sized in x and t
 * 
   * u_{xx} + u_{tt} + c(x, t)=0
   * 
   * a: (Double, Double) => Double,
   b: (Double, Double) => Double, 
   c: (Double, Double) => Double,
   f: (Double, Double) => Double, 
   */

class LinearPDE2 (a: (Double, Double) => Double,
                 b: (Double, Double) => Double, 
                 c: (Double, Double) => Double,
                 d: (Double, Double) => Double,
                 e: (Double, Double) => Double,
                 f: (Double, Double) => Double,
                 g: (Double, Double) => Double)
                 extends PDE
{
  def generateSolution (V: Boundary, xstep: Double, tstep: Double) : Array[Array[Double]] = {
    
    def assertBoundary {
      
    }

    val step = xstep
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
         f(xmin+i*step,tmin+j*step)*step*step )/4.0
      }
      var diff = 0.0 //Difference between iterations
      
      for(i <- 1 until xsize-1){
	for(j <- 1 until tsize-1){
          v(i)(j) = (u(i-1)(j) + u(i+1)(j) + u(i)(j-1) + u(i)(j+1) -
                     f(xmin+i*xstep,tmin+j*tstep)*step*step )/4.0
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


object LinearPDE {
  

}
