package pde

import pde.expression._
import pde.variable.Const
class PDE2 extends PDE 

object PDE2 {
  
  def apply (a: Expr, b: Expr, c: Expr, f: Expr, g: Expr, h: Expr, psi: Expr) : PDE
  	= (c, f, g, h, psi) match{
    case (Zero, Zero, Zero, Zero, Zero) => if (a == Const(1) && b == Const(1)) new Laplace2 else 
      new LinearPDE2(a, b, c, f, g, h, psi)
    case _ =>  new LinearPDE2(a, b, c, f, g, h, psi)
  }
  
}

class LinearPDE2(a: Expr, b: Expr, c: Expr, f: Expr, g: Expr, h: Expr, psi: Expr) extends PDE2 {
   
}

class Laplace2 extends PDE2