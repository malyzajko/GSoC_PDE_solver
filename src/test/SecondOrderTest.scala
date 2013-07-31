object SecondOrderTest extends App {
  import pde._
  import pde.expression._
  import pde.boundary._
  import pde.expression.Expression.double2Const
  import scala.math.Pi
  
  val x = new NonFunctionVariable("x")
  val t = Variable("t")
  val u = new FunctionVariable("u", x, t)
  val laplace = dd(u, x, x) + dd(u, t, t) === Const(0)
  val boundary = new RectBoundary(
    new BFunction(fixVar(t, 0), From(0, 20), condition(u, 0)),
    new BFunction(fixVar(t, 10), From(0, 20), condition(u, Sin(Pi * x/20)*5)),
    new BFunction(fixVar(x, 0), From(0, 10), condition(u, 0)),
    new BFunction(fixVar(x, 20), From(0, 10), condition(u, 0))
  )
  val (solution, err) = Solver.solve(laplace, boundary)
  
  val realLSolution = (x: Double, t: Double) =>
  (5/scala.math.sinh(Pi/2)) * scala.math.sin(Pi * x / 20) * scala.math.sinh(Pi*t/20)
  
  println("Generated Point (4, 2): " + solution(20)(20) + " +/- " + err(20)(20))
  println("Real: " + realLSolution(4,2))
  println("Generated Point(18, 9): " + solution(90)(90) + " +/- " + err(90)(90))
  println("Real: " + realLSolution(18,9))
  println("Generated Point(10, 5): " + solution(50)(50) + " +/- " + err(50)(50))
  println("Real: " + realLSolution(10,5))
  println("Generated Point(19.8, 9.8): " + solution(99)(99) + " +/- " + err(99)(99))
  println("Real: " + realLSolution(19.8,9.8))
  
  //Heat Equation
  println("\nHeat Equation:\n")
  val heqn = d(u, t) === 4*dd(u, x, x)
  val boundary2 = new RectBoundary(
    new BFunction(fixVar(t, 0), From(0, 2), condition(u, Sin(3*Pi*x))),
    new BFunction(fixVar(t, 1), From(0, 2), condition(u, 0)),
    new BFunction(fixVar(x, 0), From(0, 1), condition(u, 0)),
    new BFunction(fixVar(x, 1), From(0, 1), condition(u, 0))
  )
  
  val (solution2, err2) = Solver.solve(heqn, boundary2)
  val realHSolution = (y: Double, z: Double) => scala.math.sin(2*Pi*y)*scala.math.exp(-4*Pi*Pi*z)

  println("Generated Point (0.1, 0.5): " + solution2(1)(1))
  println("Real: " + realHSolution(0.01, 0.5))
}
