object SecondOrderTest extends App {
  import pde._
  import pde.expression._
  import pde.boundary._
  import pde.expression.Expression.double2Const
  import scala.math.Pi
  
  val x = new NonFunctionVariable("x")
  val t = Variable("t")
  val u = new FunctionVariable("u", x, t)
//  val laplace = dd(u, x, x) + dd(u, t, t) === Const(0)
  val boundary = new RectBoundary(
    new BFunction(fixVar(t, 0), From(0, 10), condition(u, 0)),
    new BFunction(fixVar(t, 20), From(0, 10), condition(u, Sin(Pi * x/20)*5)),
    new BFunction(fixVar(x, 0), From(0, 20), condition(u, 0)),
    new BFunction(fixVar(x, 20), From(0, 20), condition(u, 0))
  )
  /*
   val (solution, err) = Solver.solve(laplace, boundary)

   val realSolution = (x: Double, t: Double) =>
   (5/scala.math.sinh(Pi/2)) * scala.math.sin(Pi * x / 20) * scala.math.sinh(Pi*t/20)

   println("Generated Point (0, 0): " + solution(0)(0))
   println("Real: " + realSolution(0,0))
   println("Generated Point(5, 5): " + solution(5)(5))
   println("Real: " + realSolution(5,5))
   */  
}
