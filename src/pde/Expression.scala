package pde.expression;

import scala.Math.pow
import pde.variable._
import pde.PDE
import Math._
import collection.immutable.HashMap

case class CannotEvaluateException extends Exception
  
  
abstract class Expr {
     
    def ^(e: Expr) = Pow(this, e)
    def *(e: Expr) = Mul(this, e)
    def /(e: Expr) = Div(this, e)
    def +(e: Expr) = Add(this, e)
    def -(e: Expr) = Sub(this, e)
    
    def eval(vars: Map[NFVariable, Double]) = {
       def evalapp(e: Expr): Double = e match {
	      case Zero => 0
	      case Pow(a, b) => pow(evalapp(a), evalapp(b))
	      case Mul(a, b) => evalapp(a) * evalapp(b)
	      case Div(a, b) => evalapp(a) / evalapp(b)
	      case Add(a, b) => evalapp(a) + evalapp(b)
	      case Sub(a, b) => evalapp(a) - evalapp(b)
	      case Neg(a)    => - evalapp(a)
	      case x @ NFVariable(name) => vars(x)
	      case Const(c) => c
	      case FVariable(_) => throw new CannotEvaluateException
	      case d(u, x) => throw new CannotEvaluateException
	      case dd(u, x, t) => throw new CannotEvaluateException
       }	
       evalapp(this)
       
       //TODO test speed of this function
    }
    
    def infix_=(e: Expr): PDE  = {
      import scala.collection.mutable.Map
      val exp = Sub(this, e)
      val map = Map[Function, Expr]()
      
      def update(u: Function, e: Expr) {
        if (map.contains(u)) {
          val old = map(u)
          map += (u -> Add(old, e))
        } else 
          map += (u -> e)
      }
      
      object noDerivative extends Function
      
      def splitUp(e: Expr){
        e match {
	      case Mul(a, b: Derivative) => update(b, a)
	      case Add(a: Derivative, b) => update(a, Const(1)); splitUp(b)
	      case Sub(a: Derivative, b) => update(a, Const(1)); splitUp(Neg(b))
	      case a: Derivative         => update(a, Const(1))
	      case f @ Add(a, b)         => update(noDerivative, f) 
	      case f @ Sub(a, b)         => update(noDerivative, Neg(f)) 
        }
      }
      
      splitUp(exp)
      val order = {
        def orderApp(e: Iterable[Function], o: Int): Int = e match {
          case dd(_, _, _)::xs => if (o>2) orderApp(xs, o) else orderApp(xs, 2)
          case d(_, _)::xs =>  if (o>1) orderApp(xs, o) else orderApp(xs, 1)
          case x::xs => orderApp(xs, 0)
          case Nil => o
        }
        orderApp(map.keys, 0)
      }
      
   
      val u = map.first._1 match {
        case f: FVariable => f
        case d(f, _) => f
        case dd(f, _, _) => f
      }
            
      order match {
        case 1 => {
          val c = if (map.contains(u)) map(u) else Zero
          val a = if (map.contains( d(u, u.vars(1)) ) ) map(d(u, u.vars(1))) else Zero
          val b = if (map.contains(d(u, u.vars(2)))) map(d(u, u.vars(2))) else Zero
          val f = if (map.contains(noDerivative)) map(noDerivative) else Zero
          PDE.firstOrder(a, b, c, f)
          }
        case 2 => {
          val psi = if (map.contains(noDerivative)) map(noDerivative) else Zero
          val h = if (map.contains(u)) map(u) else Zero
          val g = if (map.contains( d(u, u.vars(1)) ) ) map(d(u, u.vars(1))) else Zero
          val f = if (map.contains(d(u, u.vars(2)))) map(d(u, u.vars(2))) else Zero
          val c = if (map.contains( dd(u, u.vars(1), u.vars(2)) ) ) map(dd(u, u.vars(1), u.vars(2))) else Zero
          val b = if (map.contains(dd(u, u.vars(1), u.vars(1)) )) map(dd(u, u.vars(1), u.vars(1))) else Zero
          val a = if (map.contains(dd(u, u.vars(2), u.vars(2)) )) map(dd(u, u.vars(2), u.vars(2))) else Zero
          PDE.secondOrder(a, b, c, f, g, h, psi)
        }
      }
    }
    
}
 
case object Zero extends Expr
case class Pow(a: Expr, b: Expr) extends Expr
case class Mul(a: Expr, b: Expr) extends Expr
case class Div(a: Expr, b: Expr) extends Expr
case class Add(a: Expr, b: Expr) extends Expr
case class Sub(a: Expr, b: Expr) extends Expr
case class Neg(a: Expr) extends Expr
case class LEQ(a: Double, x: Variable) extends Expr {
  def <(c: Double) = LEQexp(x, a, c)
}
case class LEQexp(variable: Variable, lower: Double, upper: Double){
  assert(lower < upper)
}

abstract class Derivative extends Expr with Function
case class d(u: FVariable, x: NFVariable) extends Derivative
case class dd(u: FVariable, x: NFVariable, t: NFVariable) extends Derivative

object Expression{
  
  implicit def double2Const(c: Double) = Const(c)

  
  def eval(e: Expr, vars: Map[NFVariable, Double]): Double = {
    def evalapp(e: Expr): Double = e match {
      case Zero => 0
      case Pow(a, b) => pow(evalapp(a), evalapp(b))
      case Mul(a, b) => evalapp(a) * evalapp(b)
      case Div(a, b) => evalapp(a) / evalapp(b)
      case Add(a, b) => evalapp(a) + evalapp(b)
      case Sub(a, b) => evalapp(a) - evalapp(b)
      case Neg(a)    => - evalapp(a)
      case x @ NFVariable(name) => vars(x)
      case Const(c) => c
      case FVariable(_) => throw new CannotEvaluateException
      case d(u, x) => throw new CannotEvaluateException
      case dd(u, x, t) => throw new CannotEvaluateException
    }	
    evalapp(e)
  }
  
}