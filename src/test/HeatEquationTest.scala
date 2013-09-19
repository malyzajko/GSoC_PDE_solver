object HeatEquationTest extends App {
  import pde._
  import pde.model.expression._
  import pde.model.{Boundary, RectBoundary, ThreeSidedBoundary}
  import scala.math.{Pi, sin, exp}
  import pde.model.expression.Expr.{double2Const, ciT2BFunction}
  import pde.model.expression.Expr._
  import pde.model.expression.{to}
  import pde.solver.Solver
  import pde.model.{LinearPDE1, HeatEquation}
  import ceres.smartfloat.SmartFloat
  import scala.util.Random
  val L = 1
  val twoL = 2
  val t = new Variable("t")
  val x = new Variable("x")
  val u = new FunctionVariable("u", x, t)
  val heatEquation = d(u, t) := 5 * dd(u, x, x)
  println(heatEquation)
  val boundary = new ThreeSidedBoundary(
    (u(x, 0) := Sin(Pi*x/L) * 6, from(0 to L)),
    (u(0, t) := 0, from(0 to 2*L)),
    (u(L, t) := 0, from(0 to 2*L))
  )
  val realSolution = (x: Double, t: Double) =>
    6*sin(Pi*x/L)*exp(-5 * (Pi/L)*(Pi/L) *t)
  println("Start")
  val solution = Solver.solve(heatEquation, boundary)
  val random = new Random()
  println("End")
  for(i <- 0 until 1000; if i%100==0){
    val xvalue = i.toDouble/1000
    println("Generated Point ("+ xvalue + ", 0.002): " + solution(i)(1).d + "\n"
            + solution(i)(1))
    println("Real: " + realSolution(xvalue, 0.002))
  }
  for(i <- 0 to 30) {
    val xval = (scala.math.floor (random.nextDouble * 1250))/1000
    val tval = (scala.math.floor (random.nextDouble * 2500))/1000
    val xindex = (xval*1250).toInt
    val tindex = (tval*625).toInt
    
    println("Generated Point (" + xval + ", " + tval + "): "
            + solution(xindex)(tindex).d + "\n" + solution(xindex)(tindex))
    println("Real: " + realSolution(xval, tval))
  }

}
