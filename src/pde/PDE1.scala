package pde

import pde.expression._

class PDE1 extends PDE 

object PDE1 {
  
  def apply (a: Expr, b: Expr, c: Expr, d: Expr) = new LinearPDE1(a, b, c, d)
  
}

class LinearPDE1(a: Expr, b: Expr, c: Expr, d: Expr) extends PDE1 {
  
  
  
}