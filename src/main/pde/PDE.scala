package pde

trait PDE {
	
}

object PDE {
  import pde.expression._
  
  def firstOrder(map: scala.collection.mutable.Map[Function, Expr]) = PDE1(map)
  def secondOrder(map: scala.collection.mutable.Map[Function, Expr]) :PDE
  = PDE2(map)
  
}
