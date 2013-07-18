package unitTest

object expTest {
  import pde.expression._
  import pde.expression.Expression._
  import pde.variable._
  import Math._
  import collection.immutable.HashMap
  
  val x = new NFVariable("x")
  val t = new NFVariable("t")
  val u = new FVariable("u")
  val map = collection.immutable.Map(x -> 2.5, t -> 3.0)
  val exp1 = Add(x, Add(x, Mul(t,5)))
  
  println(eval(exp1, map))
}