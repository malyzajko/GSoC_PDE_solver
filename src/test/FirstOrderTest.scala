object FirstOrderTest extends App {
  import pde._
  import pde.expression._
  import pde.boundary._
  import scala.math.Pi
  import pde.expression.Expr.{double2Const, ciT2BFunction}
  import pde.expression.Expr._
  import pde.expression.{to}

  val x = Variable("x")
  val t = Variable("t")
  val u = new FunctionVariable("u", x, t)
  val testPDE = t*d(u, t) + x*d(u, x)  - u := 0
  val boundary = new RectBoundary(
    (u(0, t) := 3*t, to(0, 10)),
    (u(10, t):=20+3*t, to(0, 10)),
    (u(x, 0) := 2*x, to(0, 10)),
    (u(x, 10):= 2*x+30, to(0, 10))
  )
  val realSolution = (x: Double, t: Double) =>2*x+3*t
  val solution = Solver.solve(testPDE, boundary)
  println("Generated Point (0.1, 0.1): " + solution(1)(1))
  println("Real: " + realSolution(0.1,0.1))
  println("Generated Point (1, 1): " + solution(10)(10))
  println("Real: " + realSolution(1,1))
  println("Generated Point(5, 5): " + solution(50)(50))
  println("Real: " + realSolution(5,5))
  println("Generated Point(9.9, 9.9): " + solution(99)(99))
  println("Real: " + realSolution(9.9,9.9))

}
