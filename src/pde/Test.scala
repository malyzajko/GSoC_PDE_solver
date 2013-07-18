package pde;

object Test extends App {

    /*
     * testing with the Laplace equation : a u_{xx} + u_{tt} = 12x+10t 
     * and a boundary with constant potential along all sides except for
     * at the bottom
     *
    val x = new NFVariable("x")
    val t = new NFVariable("t")
    val u = new FVariable("u", x, t)
    
    val bot = (x: Double, t: Double) => x*x*x + t*t*t + 2.0*x*x*t + 3.0*x*t*t + 4.0*x + 5.0*t +6.0
    val one = (x: Double, t : Double) => 1.0
    val zero = (x: Double, t:Double ) => 0.0
    val testBoundary = new Boundary(
      (bot, (-1, 1), (1, -1)),
      (bot, (-1, 1), (1, 1)),
      (bot, (-1, 1), (0, -1)),
      (bot, (-1, 1), (0, 1))
    )
    def solution (x: Double, t: Double) = sin(Math.Pi*x)*exp(-Math.Pi*t)
      val test = new LinearPDE2(one, zero, one, zero, zero, zero, (x, t) => 12*x + 10* t)
      val result = test.generateSolution(testBoundary, 0.2, 0.2)
    for (i <- 0 until result.size){
      for (j <- 0 until result(1).size) {
        printf(" %2.2f ",result(i)(j))
      }
      println("")
    }
    */
	import collection.immutable.HashMap
	import pde.variable._
	import pde.expression.Expression._
	import pde.expression._;

	val x = new NFVariable("x")
	val t = new NFVariable("t")
	val u = new FVariable("u")
	val map = collection.immutable.Map(x -> 2.5, t -> 3.0)
	val exp1 = Add(x, Add(x, Mul(t,5)))
	  
	println(eval(exp1, map))
    
    
}