package pde

trait PDE {
	
}

object PDE {
  import pde.expression._
  
  private def apply(a: Expr, b:Expr, n: Int) = n match {
    case 1 => 0
  }
  
  def firstOrder(a: Expr, b: Expr, c: Expr, f: Expr) = PDE1(a, b, c, f)
  def secondOrder(a: Expr, b: Expr, c: Expr, f: Expr, g: Expr, h: Expr, psi: Expr) :PDE
  	= PDE2(a, b, c, f, g, h, psi)
  	
}