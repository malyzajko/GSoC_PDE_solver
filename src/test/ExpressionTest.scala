import org.scalatest._

object ExpressionTest extends App {
  import pde.model.expression.Expr.double2Const
  import pde.model.expression._
  import ceres.smartfloat.SmartFloat
  
  val x = new Variable("x")
  val t = new Variable("t")
  val (testPointx, testPointt) = ((x,  1.0),(t, 4.0))
    val exp1 = x
  val exp2 = x*t + x*x+ 4
  println(exp2.eval(testPointx, testPointt))
  println(4.eval(testPointx, testPointt))
  def testExp() {
    assert(exp2.eval(testPointx, testPointt).d == 9.0)
  }
}
