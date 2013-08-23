//TBD
package pde

trait PDE

case class NotAPDEException()        extends Exception

object PDE {
  import pde.expression.{Function, Expr}

  def firstOrder(map: scala.collection.immutable.Map[Function, Expr]): PDE1 = PDE1(map)
  def secondOrder(map: scala.collection.immutable.Map[Function, Expr]) : PDE2
  = PDE2(map)

}
