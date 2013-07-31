package pde.expression;

import pde.PDE
import Math._
import collection.immutable.HashMap

case class CannotEvaluateException() extends Exception


sealed abstract class Expr {
  
  def ^(e: Expr) = this match {
    case f: Function => Powf(f, e)
    case _           => Pow(this, e)
  }
  def *(e: Expr) = Mul(this, e)
  def /(e: Expr) = Div(this, e)
  def +(e: Expr) = Add(this, e)
  def -(e: Expr) = Sub(this, e)

  def eval(vars: Map[NonFunctionVariable, Double]) = {
    def evalapp(e: Expr): Double = e match {
      case Zero => 0
      case Pow(a, b) => pow(evalapp(a), evalapp(b))
      case Mul(a, b) => evalapp(a) * evalapp(b)
      case Div(a, b) => evalapp(a) / evalapp(b)
      case Add(a, b) => evalapp(a) + evalapp(b)
      case Sub(a, b) => evalapp(a) - evalapp(b)
      case Sin(a)    => scala.math.sin(evalapp(a))
      case Neg(a)    => - evalapp(a)
      case x @ NonFunctionVariable(name) => vars(x)
      case Const(c) => c
      case FunctionVariable(_,_,_) => throw new CannotEvaluateException
      case d(u, x) => throw new CannotEvaluateException
      case dd(u, x, t) => throw new CannotEvaluateException
      case _ => throw new CannotEvaluateException
    }
    evalapp(this)

    //TODO test speed of this function
  }
  
  def ===(e: Expr): PDE  = {
    import scala.collection.mutable.Map
    val exp = Sub(this, e)
    val map = Map[Function, Expr]()

    def hasFunction(e: Expr): Boolean = e match {
      case Pow(a, b) => hasFunction(a) || hasFunction(b)
      case Mul(a, b) => hasFunction(a) || hasFunction(b)
      case Add(a, b) => hasFunction(a) || hasFunction(b)
      case Sub(a, b) => hasFunction(a) || hasFunction(b)
      case Div(a, b) => hasFunction(a) || hasFunction(b)
      case f: Function => true
      case _ => false
    }
    def update(u: Function, e: Expr) {

      if (map.contains(u)) {
        val old = map(u)
        map += (u -> Add(old, e))
      } else
        map += (u -> e)
    }

    def splitUp(e: Expr){
      e match {
        case Mul(a, f: Function)          => update(f, a)
        case Mul(a, Powf(f: Function, c)) => update(Powf(f, c), a)
        case Mul(a, b)                    => update(noFunction, Mul(a, b))
	case Add(a: Function, b)          => update(a, Const(1)); splitUp(b)
	case Add(a, b)                    => splitUp(a); splitUp(b)
	case Sub(a: Function, b)          => update(a, Const(1)); splitUp(Neg(b))
	case Sub(a, b)               => {
          if (hasFunction(a)) {splitUp(a); splitUp(Neg(b))}
          else update(noFunction, Neg(Add(a, b)))}
        case Neg(a: Function)             => update(a, Neg(Const(1)))
        case Neg(a)                       => splitUp(a)
        case f: Function                  => update(f, Const(1))
        case Powf(f: Function, c)         => update(Powf(f, c), Const(1))
        case Const(0)                     => update(noFunction, Zero)
        case _                            => update(noFunction, e)
      }
    }
    
    splitUp(exp)
    val order = {
      def orderApp(e: List[Function], o: Int): Int = e match {
        case dd(_, _, _)::xs => if (o>2) orderApp(xs, o) else orderApp(xs, 2)
        case d(_, _)::xs =>  if (o>1) orderApp(xs, o) else orderApp(xs, 1)
        case x::xs => orderApp(xs, o)
        case Nil => o
      }
      orderApp(map.keys.toList, 0)
    }

    val u = {
      def getFunction(kss: List[Function]): FunctionVariable = kss match {
        case (f: FunctionVariable)::ks => f
        case d(f, _)::ks               => f
        case dd(f, _, _)::ks           => f
        case k::ks                     => getFunction(ks)
      }
      getFunction(map.keys.toList)
    }

    order match {
      case 1 => {
        PDE.firstOrder(map)
      }
      case 2 => {
        val nofunct = if (map.contains(noFunction)) map(noFunction) else Zero
        val noOrder = if (map.contains(u)) map(u) else Zero
        val dx = if (map.contains( d(u, u.x) ) ) map(d(u, u.x)) else Zero
        val dt = if (map.contains(d(u, u.t))) map(d(u, u.t)) else Zero
        val dxt = if (map.contains( dd(u, u.x, u.t) ) && map.contains(dd(u, u.x, u.t)))
          Add(map(dd(u, u.x, u.t)), map(dd(u, u.t, u.x)))
        else if (map.contains(dd(u, u.x, u.t))) map(dd(u, u.x, u.t))
        else if (map.contains(dd(u, u.t, u.x))) map(dd(u, u.t, u.x))
        else Zero
        val dtx = if (map.contains( dd(u, u.x, u.t) ) ) map(dd(u, u.x, u.t)) else Zero
        val dxx = if (map.contains(dd(u, u.x, u.x) )) map(dd(u, u.x, u.x)) else Zero
        val dtt = if (map.contains(dd(u, u.t, u.t) )) map(dd(u, u.t, u.t)) else Zero
          (dtt, dxx, dtx, dxt, dt, dx, noOrder, nofunct) match {
          case (Const(1.0), Const(1.0), Zero, Zero, Zero, Zero, Zero, Zero) => new pde.Laplace2(u)
          case _ => PDE.secondOrder(map)
        }
      }
    }
  }

}

case object Zero extends Expr
object noFunction extends Function
case class Powf(f: Function, c: Expr) extends Expr with Function
case class Pow(a: Expr, b: Expr) extends Expr
case class Mul(a: Expr, b: Expr) extends Expr
case class Div(a: Expr, b: Expr) extends Expr
case class Add(a: Expr, b: Expr) extends Expr
case class Sub(a: Expr, b: Expr) extends Expr
case class Sin(a: Expr) extends Expr
case class Neg(a: Expr) extends Expr

trait Function

abstract class Derivative extends Expr with Function
case class d(u: FunctionVariable, x: NonFunctionVariable) extends Derivative
case class dd(u: FunctionVariable, x: NonFunctionVariable, t: NonFunctionVariable) extends Derivative

object Variable {

  private var Uniquecounter = 1
  
  def apply(x: String) = new NonFunctionVariable(x)
  
}

class invalidVariableException(x: Variable) extends RuntimeException

sealed abstract class Variable extends Expr {
  def apply(x: String) = new NonFunctionVariable("x")
  def unnaply(x: NonFunctionVariable) = x
}


case class NonFunctionVariable(xname: String) extends Variable{

  val x = Symbol(xname)
}

case class FunctionVariable(uname: String, x: NonFunctionVariable, t: NonFunctionVariable)
    extends Variable with Function {
  
  val u = Symbol(uname)
  
}

case class Const(c: Double) extends Variable{
  def *(k: Double) = Const(c*k)
  def /(k: Double) = Const(c/k)
  def +(k: Double) = Const(c+k)
  def -(k: Double) = Const(c-k)
}

case class fixVar(variable: NonFunctionVariable, c: Double)
case class From(a: Double, b: Double){assert(a<b)}
case class condition(function: FunctionVariable, exp: Expr)
case class Point(coord: Map[NonFunctionVariable, Double]){
  def apply(x: NonFunctionVariable) = coord(x)
}

case class BFunction(fixed: fixVar, interval: From, u: condition) {
  def lowerPoint = if (fixed.variable == u.function.x)
    Point(Map(u.function.x -> fixed.c, u.function.t -> interval.a))
  else Point(Map(u.function.x -> interval.a, u.function.t -> fixed.c))
  def upperPoint = if (fixed.variable == u.function.x)
    Point(Map(u.function.x -> fixed.c, u.function.t -> interval.b))
  else Point(Map(u.function.x -> interval.b, u.function.t -> fixed.c))

  def lowerValue = if (fixed.variable == u.function.x)
    u.exp.eval(Map(u.function.x -> fixed.c, u.function.t -> interval.a))
  else u.exp.eval(Map(u.function.x -> interval.a, u.function.t -> fixed.c))
  def upperValue = if (fixed.variable == u.function.x)
    u.exp.eval(Map(u.function.x -> fixed.c, u.function.t -> interval.b))
  else u.exp.eval(Map(u.function.x -> interval.b, u.function.t -> fixed.c))

  
}

object Expression{
  
  implicit def double2Const(c: Double) = Const(c)
  
}
