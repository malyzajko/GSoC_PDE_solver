package pde.expression;

import pde.PDE
import Math._
import collection.immutable.HashMap
import ceres.smartfloat.{SmartFloat}


case class CannotEvaluateException() extends Exception


sealed abstract class Expr {
  import Expr.{double2Const}
  import ceres.smartfloat.SmartFloat.double2SmartFloat

  def ^(e: Expr) = this match {
    case f: Function => Powf(f, e)
    case _           => Pow(this, e)
  }
  def *(e: Expr) = Mul(this, e)
  def /(e: Expr) = Div(this, e)
  def +(e: Expr) = Add(this, e)
  def -(e: Expr) = Sub(this, e)

  def eval(vars: Map[Variable, Double]) = {
    def evalapp(e: Expr): SmartFloat = e match {
      case Zero => 0
      case Pow(a, b) => SmartFloat.pow(evalapp(a), evalapp(b))
      case Mul(a, b) => evalapp(a) * evalapp(b)
      case Div(a, b) => evalapp(a) / evalapp(b)
      case Add(a, b) => evalapp(a) + evalapp(b)
      case Sub(a, b) => evalapp(a) - evalapp(b)
      case Sin(a)    => SmartFloat.sin(evalapp(a))
      case Neg(a)    => - evalapp(a)
      case x @ Variable(name) => SmartFloat(vars(x))
      case Const(c: SmartFloat) => c
      case FunctionVariable(_,_,_) => throw new CannotEvaluateException
      case d(u, x) => throw new CannotEvaluateException
      case dd(u, x, t) => throw new CannotEvaluateException
      case _ => throw new CannotEvaluateException
    }
    evalapp(this)

    //TODO test speed of this function
  }

  def :=(e: Expr): PDE  = {
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
    def update(u: Function, e: Expr) = e match {
      case Zero => ()
      case _    =>
        if (map.contains(u)) {
          val old = map(u)
          map += (u -> Add(old, e))
        } else
          map += (u -> e)
    }

    def splitUp(e: Expr){
      e match {
        case Mul(a, Powf(f: Function, c)) => update(Powf(f, c), a)
        case Mul(a, f: Function)          => update(f, a)
        case Mul(a, b)                    => update(noFunction, Mul(a, b))
        case Add(a: Function, b)          => update(a, Const(1)); splitUp(b)
        case Add(a, b)                    => splitUp(a); splitUp(b)
        case Sub(a: Function, b)          => update(a, Const(1)); splitUp(Neg(b))
        case Sub(a, b)                    => {
          if (hasFunction(a)) {splitUp(a); splitUp(Neg(b))}
          else update(noFunction, Neg(Add(a, b)))}
        case Neg(a: Function)             => update(a, Neg(Const(1)))
        case Neg(Mul(b, a: Function))     => update(a, Neg(b))
        case Neg(a)                       => splitUp(a)
        case Powf(f: Function, c)         => update(Powf(f, c), Const(1))
        case f: Function                  => update(f, Const(1))
        case Zero                         => ()
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
        case Nil                       => throw new Exception
        case (f: FunctionVariable)::ks => f
        case d(f, _)::ks               => f
        case dd(f, _, _)::ks           => f
        case k::ks                     => getFunction(ks)
      }
      getFunction(map.keys.toList)
    }

    val imap = map.toMap

    order match {
      case 1 => {
        PDE.firstOrder(imap)
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
        val dxx = if (map.contains(dd(u, u.x, u.x) )) map(dd(u, u.x, u.x)) else Zero
        val dtt = if (map.contains(dd(u, u.t, u.t) )) map(dd(u, u.t, u.t)) else Zero
          (dxx, dxt, dtt, dt, dx, noOrder, nofunct) match {
          case (a, Zero, b, Zero, Zero, Zero, c) =>
            if (a == Const(1.0) && b == Const(1.0) && c == Zero) new pde.Laplace2(u)
            else new pde.Poisson2(u, c)
          case (Const(a), Const(b), Const(c), d, e, f, g) =>
            if (c*c < 4*a*b) new pde.Elliptical(Const(a), Const(b), Const(c), d, e, f, g, u)
            else if (c*c == 4*a*b) new pde.Parabolic(Const(a), Const(b), Const(c), d, e, f, g, u)
            else new pde.Hyperbolic(Const(a), Const(b), Const(c), d, e, f, g, u)
          case _ => PDE.secondOrder(dtt, dxt, dxx, dt, dx, noOrder, nofunct, u)
        }
      }
    }
  }
}


object noFunction extends Function
case class Powf(f: Function, c: Expr) extends Expr with Function
case class Pow(a: Expr, b: Expr) extends Expr
case class Mul(a: Expr, b: Expr) extends Expr
case class Div(a: Expr, b: Expr) extends Expr
case class Add(a: Expr, b: Expr) extends Expr
case class Sub(a: Expr, b: Expr) extends Expr
case class Sin(a: Expr) extends Expr
case class Neg(a: Expr) extends Expr

class Const(val c : SmartFloat) extends Expr {
  def *(k: SmartFloat) = Const(c*k)
  def /(k: SmartFloat) = Const(c/k)
  def +(k: SmartFloat) = Const(c+k)
  def -(k: SmartFloat) = Const(c-k)
}

object Const {
  def apply(c: SmartFloat): Const ={
    if (c == 0.0) Zero
    else new Const(c)
  }
  def apply(c: Double): Const = {
    if (c == 0.0) Zero
    else new Const(SmartFloat(c))
  }
  def unapply(c: Const): Option[SmartFloat] = Some(c.c)
}

case object Zero extends Const(0.0)

trait Function

abstract class Derivative extends Expr with Function
case class d(u: FunctionVariable, x: Variable) extends Derivative
case class dd(u: FunctionVariable, x: Variable, t: Variable) extends Derivative

object Variable {

  private var Uniquecounter = 1

}

class invalidVariableException(x: Variable) extends RuntimeException

sealed case class Variable(xname: String) extends Expr


case class FunctionVariable(uname: String, x: Variable, t: Variable)
    extends Expr with Function {
  val u = Symbol(uname)
  def apply(c: Double, variable: Variable): fixedVar = {
    assert(variable == t)
    fixedVar(this, x, c)
  }
  def apply(variable: Variable, c: Double): fixedVar = {
    assert(variable == x)
    fixedVar(this, t, c)
  }
}

case class fixedVar(u: FunctionVariable, variable: Variable, c: Double) {
  // useage looks like u(0, t) :=6+5*t
  def :=(e: Expr): Condition = Condition(u, variable, c, e)
}

sealed abstract class Interval{
  val lo: Double
  val hi: Double
}
final case class to(val lo: Double, val hi: Double) extends Interval {(assert(lo<hi))}
case class Condition(function: FunctionVariable, fixed: Variable, c: Double, exp: Expr)
case class Point(coord: Map[Variable, Double]){
  def apply(x: Variable): Double = coord(x)
}

case class BFunction(u: Condition, interval: Interval) {

  def lowerPoint = if (u.fixed == u.function.x)
    Point(Map(u.function.x -> u.c, u.function.t -> interval.lo))
  else Point(Map(u.function.x -> interval.lo, u.function.t -> u.c))
  def upperPoint = if (u.fixed == u.function.x)
    Point(Map(u.function.x -> u.c, u.function.t -> interval.hi))
  else Point(Map(u.function.x -> interval.hi, u.function.t -> u.c))

  def lowerValue = if (u.fixed == u.function.x)
    u.exp.eval(Map(u.function.x -> u.c, u.function.t -> interval.lo))
  else u.exp.eval(Map(u.function.x -> interval.lo, u.function.t -> u.c))
  def upperValue = if (u.fixed == u.function.x)
    u.exp.eval(Map(u.function.x -> u.c, u.function.t -> interval.hi))
  else u.exp.eval(Map(u.function.x -> interval.hi, u.function.t -> u.c))


}

object Expr{

  implicit def double2Const(c: Double) = Const(SmartFloat(c))
  implicit def ciT2BFunction(bf: Tuple2[Condition, Interval]): BFunction =
    BFunction(bf._1, bf._2)

}
