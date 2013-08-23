//TBD
package pde.expression;

import pde.PDE
import Math.{pow, sin}
import collection.immutable.Map
import scala.collection.mutable.{Map => MutableMap}
import pde.errorVal.{ErrorVal, AffineVal}
import pde.errorVal.AffineVal.AffineCoefficient

case class CannotEvaluateException() extends Exception


sealed abstract class Expr {

  def ^(e: Expr): Expr = this match {
    case f: Function => Powf(f, e)
    case _           => Pow(this, e)
  }
  def *(e: Expr): Expr = Mul(this, e)
  def /(e: Expr): Expr = Div(this, e)
  def +(e: Expr): Expr = Add(this, e)
  def -(e: Expr): Expr = Sub(this, e)

  def eval(vars: Map[Variable, Double]): AffineVal = {
    def evalapp(e: Expr): AffineVal = e match {
      case Zero => AffineVal(0.0)(Array(AffineCoefficient(Math.pow(2, -53), "roundOff")))
      case Pow(a, b) =>
        AffineVal(pow(evalapp(a).centralValue, evalapp(b)centralValue))(
          Array(AffineCoefficient(Math.pow(2, -53), "roundOff")))
      case Mul(a, b) => evalapp(a) * evalapp(b)
      case Div(a, b) => AffineVal(evalapp(a).centralValue / evalapp(b).centralValue)(
        Array(AffineCoefficient(Math.pow(2, -53), "roundOff")))
      case Add(a, b) => evalapp(a) + evalapp(b)
      case Sub(a, b) => evalapp(a) - evalapp(b)
      case Sin(a)    =>
        AffineVal(scala.math.sin(evalapp(a).centralValue))(
          Array(AffineCoefficient(Math.pow(2, -53), "roundOff")))
      case Neg(a)    => - evalapp(a)
      case x @ Variable(name) => AffineVal(vars(x))(
        Array(AffineCoefficient(Math.pow(2, -53), "roundOff")))
      case Const(c) => AffineVal(c)(Array(AffineCoefficient(Math.pow(2, -53), "roundOff")))
      case FunctionVariable(_,_,_) => throw new CannotEvaluateException
      case d(u, x) => throw new CannotEvaluateException
      case dd(u, x, t) => throw new CannotEvaluateException
      case _ => throw new CannotEvaluateException
    }
    evalapp(this)

    //TODO :test speed of this function, division, power, sin
    //
  }

  private def hasFunction(e: Expr): Boolean = e match {
    case Pow(a, b) => hasFunction(a) || hasFunction(b)
    case Mul(a, b) => hasFunction(a) || hasFunction(b)
    case Add(a, b) => hasFunction(a) || hasFunction(b)
    case Sub(a, b) => hasFunction(a) || hasFunction(b)
    case Div(a, b) => hasFunction(a) || hasFunction(b)
    case f: Function => true
    case _ => false
  }
  def :=(e: Expr): PDE  = {
    val exp = Sub(this, e)
    val map = MutableMap[Function, Expr]()


    def update(u: Function, e: Expr) {
      if(map.contains(u)) {
        val old = map(u)
        map += (u -> Add(old, e))
      }else { map += (u -> e) }
    }

    def splitUp(e: Expr){
      e match {
        case Mul(a, f: Function)          => update(f, a)
        case Mul(a, b)                    => update(noFunction, Mul(a, b))
        case Add(a: Function, b)          => update(a, Const(1)); splitUp(b)
        case Add(a, b)                    => splitUp(a); splitUp(b)
        case Sub(a: Function, b)          => update(a, Const(1)); splitUp(Neg(b))
        case Sub(Mul(a, b: Function),
          Mul(c, d: Function))            => update(b, a); update(d, Neg(c))
        case Sub(a, b)                    => {
          if (hasFunction(a)) {splitUp(a); splitUp(Neg(b))}
          else update(noFunction, Neg(Add(a, b)))
        }
        case Neg(Mul(a, b: Function))     => update(b, Neg(a))
        case Neg(a: Function)             => update(a, Neg(Const(1)))
        case Neg(a)                       => splitUp(a)
        case f: Function                  => update(f, Const(1))
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
        case Nil                       => throw new pde.NotAPDEException
      }
      getFunction(map.keys.toList)
    }
    val imap = map.toMap

    order match {
      case 1 => {
        PDE.firstOrder(imap)
      }
      case 2 => {
        val nofunct = if (map.contains(noFunction)) { map(noFunction) } else { Zero }
        val noOrder = if (map.contains(u)) { map(u) } else { Zero }
        val dx = if (map.contains( d(u, u.x) ) ) { map(d(u, u.x)) } else { Zero }
        val dt = if (map.contains(d(u, u.t))) { map(d(u, u.t)) } else { Zero }
        val dxt = if (map.contains( dd(u, u.x, u.t) ) && map.contains(dd(u, u.x, u.t)))
        { Add(map(dd(u, u.x, u.t)), map(dd(u, u.t, u.x))) }
        else if (map.contains(dd(u, u.x, u.t))) { map(dd(u, u.x, u.t)) }
        else if (map.contains(dd(u, u.t, u.x))) { map(dd(u, u.t, u.x)) }
        else { Zero }
        val dtx = if (map.contains( dd(u, u.x, u.t) )) { map(dd(u, u.x, u.t)) } else { Zero }
        val dxx = if (map.contains(dd(u, u.x, u.x) )) { map(dd(u, u.x, u.x)) } else { Zero }
        val dtt = if (map.contains(dd(u, u.t, u.t) )) { map(dd(u, u.t, u.t)) } else { Zero }
          (dtt, dxx, dtx, dxt, dt, dx, noOrder, nofunct) match {
          case (Const(1.0), Const(1.0), Zero, Zero, Zero, Zero, Zero, Zero) => new pde.Laplace2(u)
          case _ => PDE.secondOrder(imap)
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
case class Sin(a: Expr)          extends Expr
case class Neg(a: Expr)          extends Expr

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

case class Const(c: Double) extends Expr{
  def ^(k: Double): Const = Const(math.pow(c, k))
  def *(k: Double): Const = Const(c*k)
  def /(k: Double): Const = Const(c/k)
  def +(k: Double): Const = Const(c+k)
  def -(k: Double): Const = Const(c-k)
}

case class fixedVar(u: FunctionVariable, variable: Variable, c: Double) {
  // useage looks like u(0, t) :=6+5*t
  def :=(e: Expr): Condition = Condition(u, variable, c, e)
}
class IntervalMaker(c: Double) {
  def to(d: Double): Interval = Interval(c, d)
}
case class Interval(a: Double, b: Double){(assert(a<b))}
case class Condition(function: FunctionVariable, fixed: Variable, c: Double, exp: Expr)
case class Point(coord: Map[Variable, Double]){
  def apply(x: Variable): Double = coord(x)
}

case class BFunction(u: Condition, interval: Interval) {
  def lowerPoint: Point = if (u.fixed == u.function.x)
    Point(Map(u.function.x -> u.c, u.function.t -> interval.a))
  else Point(Map(u.function.x -> interval.a, u.function.t -> u.c))
  def upperPoint: Point = if (u.fixed == u.function.x)
    Point(Map(u.function.x -> u.c, u.function.t -> interval.b))
  else Point(Map(u.function.x -> interval.b, u.function.t -> u.c))

  def lowerValue: Double = if (u.fixed == u.function.x)
    u.exp.eval(Map(u.function.x -> u.c, u.function.t -> interval.a)).centralValue
  else u.exp.eval(Map(u.function.x -> interval.a, u.function.t -> u.c)).centralValue
  def upperValue: Double = if (u.fixed == u.function.x)
    u.exp.eval(Map(u.function.x -> u.c, u.function.t -> interval.b)).centralValue
  else u.exp.eval(Map(u.function.x -> interval.b, u.function.t -> u.c)).centralValue


}

object Expression{

  implicit def double2Const(c: Double): Const = Const(c)
  implicit def double2IntervalMaker(c: Double): IntervalMaker = new IntervalMaker(c)
  implicit def conditionIntervalTuple2BFunction(bf: Tuple2[Condition, Interval]): BFunction = BFunction(bf._1, bf._2)
}
