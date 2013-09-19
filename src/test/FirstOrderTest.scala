object FirstOrderTest extends App {
  import pde._
  import pde.model.expression._
  import pde.model.{Boundary, RectBoundary}
  import scala.math.Pi
  import pde.model.expression.Expr.{double2Const, ciT2BFunction}
  import pde.model.expression.Expr._
  import pde.model.expression.{to}
  import pde.solver.Solver
  import pde.model.LinearPDE1
  import ceres.smartfloat.SmartFloat
  
  val x = Variable("x")
  val t = Variable("t")
  val u = new FunctionVariable("u", x, t)
  val testPDE = t*d(u, t) + x*d(u, x)  - u := 0
  val boundary = new RectBoundary(
    (u(0, t) := 3*t, from(0 to 10)),
    (u(10, t):= 20+3*t, from(0 to 10)),
    (u(x, 0) := 2*x, from(0 to 10)),
    (u(x, 10):= 2*x+30, from(0 to 10))
  )

  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    ret
  }
  
  val realSolution = (x: Double, t: Double) => 2*x+3*t;
  println("running genFV")
  val result1 = time {Solver.generateFunctionVals(t, u, 1, 1, 10, 10, 0, 0)}
  println("Start")
  testPDE match {
    case a: LinearPDE1 =>{
      val solution = time {
        Solver.solveDouble(a, boundary)
      }
      val solution2 = {
        time {Solver.solve(testPDE, boundary)}
      }
      println("End")
      println("Generated Point (0.1, 0.1): " + solution(1)(1))
      println("Generated Point (0.1, 0.1): " + solution2(1)(1)++ " " + solution2(1)(1).d)
      println("Real: " + realSolution(0.1,0.1))
      println("Generated Point (1, 1): " + solution(10)(10))
      println("Generated Point (1, 1): " + solution2(10)(10) + " " + solution2(10)(10).d)
      println("Real: " + realSolution(1,1))
      println("Generated Point(5, 5): " + solution(50)(50))
      println("Generated Point(5, 5): " + solution2(50)(50) + " " + solution2(50)(50).d)
      println("Real: " + realSolution(5,5))
      println("Generated Point(9.9, 9.9): " + solution(99)(99))
      println("Generated Point(9.9, 9.9): " + solution2(99)(99) + " " + solution2(99)(99).d)
      println("Real: " + realSolution(9.9,9.9))
    }
    case _ => ()
  }
}
