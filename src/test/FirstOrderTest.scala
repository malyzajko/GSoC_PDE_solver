object FirstOrderTest extends App {
  import pde.model.expression._
  import pde.model.{Boundary}
  import pde.model.expression.Expr._
  import pde.solver.Solver
  import ceres.smartfloat.SmartFloat

  val x = Variable("x")
  val t = Variable("t")
  val u = new FunctionVariable("u", x, t)
  val testPDE = t*d(u, t) + x*d(u, x)  - u := 0
  val boundary = Boundary(
    (u(x, 0) := 2*x, from(0 to 10)),
    (u(0, t) := 3*t, from(0 to 10)),
    (u(10, t):= 20+3*t, from(0 to 10))
  )
  val boundaryR = Boundary(
    (u(x, 0) := 2*x, from(0 to 10)),
    (u(0, t) := 3*t, from(0 to 10)),
    (u(10, t):= 20+3*t, from(0 to 10)),
    (u(x, 10):= 2*x+30, from(0 to 10))
  )
  val testPDE2 = d(u, t) + d(u, x) - u := -x*t
  val boundary2 = Boundary(
    (u(x, 0) := x+2, from(0 to 10)),
    (u(0, t) := t+2, from(0 to 10)),
    (u(10, t):= 10+2+t+10*t, from(0 to 10)),
    (u(x, 10) := 10+2+x+4*x, from(0 to 10))
  )
  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    ret
  }

  val realSolution = (x: Double, t: Double) => 2*x+3*t;
  val realSolution2 = (x: Double, t: Double) => x*t+x+t+2;
  val solution2 = {
    time {Solver.solve(testPDE, boundary, xstep = 0.1, tstep = 0.1)}
  }
  println("Solution to " + testPDE)
  println("Generated Point (0.1, 0.1): " + solution2(0.1,0.1)+ "\n" + solution2(0.1, 0.1).d)
  println("Real: " + realSolution(0.1,0.1))
  println("Generated Point (0.5, 0.5): " + solution2(0.5,0.5)+ "\n" + solution2(0.5, 0.5).d)
  println("Real: " + realSolution(0.5,0.5))
  println("Generated Point (1, 1): " + solution2(1, 1) + "\n" + solution2(1, 1).d)
  println("Real: " + realSolution(1,1))
  println("Generated Point(5, 5): " + solution2(5, 5) + "\n" + solution2(5, 5).d)
  println("Real: " + realSolution(5,5))
  println("Generated Point(7.5, 7.5): " + solution2(7.5, 7.5) + "\n" + solution2(7.5, 7.5).d)
  println("Real: " + realSolution(7.5,7.5))
  println("Generated Point(9.9, 9.9): " + solution2(9.9, 9.9) + "\n" + solution2(9.9, 9.9).d)
  println("Real: " + realSolution(9.9,9.9))
  println("---------------------------------------------------------------------------------------")
  val solution3 = {
    time {Solver.solve(testPDE2, boundary2, xstep = 0.1, tstep = 0.1)}
  }
  println("Solution to " + testPDE2)
  println("Generated Point (0.1, 0.1): " + solution3(0.1,0.1)+ "\n" + solution2(0.1, 0.1).d)
  println("Real: " + realSolution2(0.1,0.1))
  println("Generated Point (0.5, 0.5): " + solution3(0.5,0.5)+ "\n" + solution2(0.5, 0.5).d)
  println("Real: " + realSolution2(0.5,0.5))
  println("Generated Point (1, 1): " + solution3(1, 1) + "\n" + solution2(1, 1).d)
  println("Real: " + realSolution2(1,1))
  println("Generated Point(5, 5): " + solution3(5, 5) + "\n" + solution2(5, 5).d)
  println("Real: " + realSolution2(5,5))
  println("Generated Point(7.5, 7.5): " + solution3(7.5, 7.5) + "\n" + solution2(7.5, 7.5).d)
  println("Real: " + realSolution2(7.5,7.5))
  println("Generated Point(9.9, 9.9): " + solution3(9.9, 9.9) + "\n" + solution2(9.9, 9.9).d)
  println("Real: " + realSolution2(9.9,9.9))

}
