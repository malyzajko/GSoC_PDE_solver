object FirstOrderTest extends App {
  import pde._
  import pde.expression._
  import pde.boundary._
  import pde.expression.Expression.double2Const
  import scala.math.Pi
  
  val x = new NonFunctionVariable("x")
  val t = Variable("t")
  val u = new FunctionVariable("u", x, t)
  val testPDE = t*d(u, t) + x*d(u, x)  - u === 0
  val boundary = new RectBoundary(
    new BFunction(fixVar(x, 0), From(0, 10), condition(u, 3*t)),
    new BFunction(fixVar(x, 10), From(0, 10), condition(u, 20+3*t)),
    new BFunction(fixVar(t, 0), From(0, 10), condition(u, 2*x)),
    new BFunction(fixVar(t, 10), From(0, 10), condition(u, 2*x+30))
  )
  val realSolution = (x: Double, t: Double) =>2*x+3*t
  val (solution, error) = Solver.solve(testPDE, boundary)
  println(solution(0)(1), solution(1)(0))
  println("Generated Point (0.1, 0.1): " + solution(1)(1) + " +/- " + error(1)(1))
  println("Real: " + realSolution(0.1,0.1))
  println("Generated Point (1, 1): " + solution(10)(10) + " +/- " + error(10)(10))
  println("Real: " + realSolution(1,1))
  println("Generated Point(5, 5): " + solution(50)(50) + " +/- " + error(50)(50))
  println("Real: " + realSolution(5,5))
  println("Generated Point(9.9, 9.9): " + solution(100)(100) + " +/- " + error(100)(100))
  println("Real: " + realSolution(9.9,9.9))

}
