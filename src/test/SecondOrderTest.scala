object SecondOrderTest extends App {
  import pde.model.expression.{Const, Variable, FunctionVariable, from, dd}
  import pde.model.{RectBoundary}
  import pde.model.expression.Expr.{double2Const, ciT2BFunction}
  import scala.math.{Pi, exp, sin}
  
  import pde.solver.Solver

  val x = new Variable("x")
  val y = Variable("y")
  val u = new FunctionVariable("u", x, y)
  val laplace = dd(u, x, x) + dd(u, y, y) := Const(0)
  val boundary = new RectBoundary(
    (u(x, 0) := 100, from(0 to 10)),
    (u(x, 10):= 0, from(0 to 10)),
    (u(0, y) := 0, from(0 to 10)),
    (u(10, y):= 0, from(0 to 10))
  )
  val solution= Solver.solve(laplace, boundary, xstep = 1, tstep = 1)
  
  val realLSolution = (x: Double, y: Double) =>
    {(400/Pi)*(exp(-Pi*y/10)*sin(Pi*x/10) +
               (1/3)*exp(-Pi*3*y/10)*sin(Pi*3*x/10) + (1/5)*exp(-Pi*y/2)*sin(Pi*x/2)
               + (1/7)*exp(-Pi*7*y/10)*sin(Pi*7*x/10) + (1/9)*exp(-Pi*9*y/10)*sin(Pi*9*x/10) +
               (1/11)*exp(-Pi*11*y/10)*sin(Pi*11*x/10) + (1/13)*exp(-Pi*13*y/10)*sin(Pi*13*x/10) +
               (1/15)*exp(-Pi*15*y/10)*sin(Pi*15*x/10) + (1/17)*exp(-Pi*17*y/10)*sin(Pi*17*x/10) +
               (1/19)*exp(-Pi*19*y/10)*sin(Pi*19*x/10) + (1/21)*exp(-Pi*21*y/10)*sin(Pi*21*x/10))}
  
  println("Generated Point (4, 2): " + solution(4, 2) +"\n"+solution(4,2).d)
  println("Real: " + realLSolution(4,2))
  println("Generated Point(9, 8): " + solution(9, 8) +"\n"+solution(9,8).d)
  println("Real: " + realLSolution(9,8))
  println("Generated Point(10, 5): " + solution(10, 5)+"\n"+solution(10,5).d )
  println("Real: " + realLSolution(10,5))
  println("Generated Point(7, 5): " + solution(7, 5)+"\n"+solution(7,5).d )
  println("Real: " + realLSolution(7,5))
  println("Generated Point(3, 7): " + solution(3, 7)+"\n"+solution(3,7).d )
  println("Real: " + realLSolution(3,7))
  println("Generated Point(6, 2): " + solution(6, 2)+"\n"+solution(6,2).d )
  println("Real: " + realLSolution(6,2))
  println("Generated Point(6.3, 2.4): " + solution(6.3, 2.4)+"\n"+solution(6.3,2.4).d )
  println("Real: " + realLSolution(6.3,2.4))
  println("Generated Point(5, 10): " + solution(5, 10)+"\n"+solution(5, 10).d )
  println("Real: " + realLSolution(5,10))
  
  

}
