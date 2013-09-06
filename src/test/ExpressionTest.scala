import org.scalatest._

class ExpressionTest extends Suite {
  import pde.errorVal.{ErrorVal, AffineDouble}
  import pde.errorVal.AffineDouble.AffineCoefficient
  import pde.expression.Expr.double2Const
  import pde.expression._

  val x = new Variable("x")
  val t = new Variable("t")
  val testPoint = scala.collection.immutable.Map(x -> 1.0, t -> 4.0)
  val exp1 = x
  val exp2 = x*t + x*x+ 4
  println(exp2.eval(testPoint))
  println(4.eval(scala.collection.immutable.Map()))
  def testExp() {
    assert(exp2.eval(testPoint).d == 9.0)
  }

}
