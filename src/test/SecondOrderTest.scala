/*
 object SecondOrderTest extends App {
 import pde._
 import pde.expression._
 import pde.boundary._
 import pde.expression.Expression.double2Const
 import scala.math.Pi
 import expression.Expression.{double2IntervalMaker => a}
 import expression.Expression.conditionIntervalTuple2BFunction

 val x = new Variable("x")
 val t = Variable("t")
 val u = new FunctionVariable("u", x, t)
 val laplace = dd(u, x, x) + dd(u, t, t) := Const(0)
 val boundary = new RectBoundary(
 (u(x, 0) := 0, a(0) to 20),
 (u(x, 10):=Sin(Pi * x/20)*5, a(0) to 20),
 (u(0, t) :=0, a(0) to 10),
 (u(20, t):=0, a(0) to 10)
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
 val heqn = d(u, t) := 4*dd(u, x, x)
 println(heqn)
 val boundary2 = new RectBoundary(
 new BFunction(u(x, 0) := double2Const(6)*Sin(Pi*x / 4), a(0) to 4),
 new BFunction(u(x, 10) := 0, a(0) to 4),
 new BFunction(u(0, t) := 0, a(0) to 10),
 new BFunction(u(4, t) := 0, a(0) to 10)
 )

 val (solution2, err2) = Solver.solve(heqn, boundary2)
 val realHSolution = (y: Double, z: Double) => 6*scala.math.sin(Pi*y/4)*scala.math.exp(-Pi*Pi*z/4)

 println("Generated Point (0.1, 0.5): " + solution2(1)(1))
 println("Real: " + realHSolution(0.01, 0.5))

 }
*/
