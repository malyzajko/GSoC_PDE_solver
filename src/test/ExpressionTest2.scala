import org.scalatest._

object ExpressionTest2 extends App {
  import pde.model.expression.Expr.double2Const
  import pde.model.expression._
  import ceres.smartfloat.SmartFloat
  import pde.solver.Solver
  
  val x = new Variable("x")
  val t = new Variable("t")
  val f = new FunctionVariable("f", x, t)
  val result = Array.ofDim[SmartFloat](100,100)
  val step = 0.01
  for(i <- 0 until 100){
    for(j <- 0 until 100){
      val xval = i * step
      val tval = i * step
      result(i)(j) = t.eval((x, xval), (t, tval))
    }
  }
  val fn = t.expr2Function2
  for(i <- 0 until 100){
    for(j <- 0 until 100){
      val xval = i * step
      val tval = i * step
      result(i)(j) = fn((x, xval), (t, tval))
    }
  }
  val result2 = Solver.generateFunctionVals(t, f, step, step, 100, 100, 0, 0)
  val result3 = Solver.generateFunctionVals(t, f, step, step, 100, 100, 0, 0)
  
  
  
}
