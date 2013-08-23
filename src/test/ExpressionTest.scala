object ExpressionTest extends App {
  import pde.expression.Expression.double2Const
  import pde.expression._

  val x = new Variable("x")
  val exp1 = x
  println(x.eval(scala.collection.immutable.Map(x -> 1)))
}
