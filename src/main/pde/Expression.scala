package pde.model.expression;

import pde.model.PDE
import Math._
import collection.immutable.HashMap
import ceres.smartfloat.{SmartFloat}
import ceres.smartfloat.SmartFloat.double2SmartFloat
import scala.collection.mutable.Map

case class CannotEvaluateException() extends Exception


sealed abstract class Expr {
  import Expr.{double2Const}

  def ^(e: Expr): Expr = this match {
    case f: Function => Powf(f, e)
    case _           => Pow(this, e)
  }
  def *(e: Expr): Expr = Mul(this, e)
  def /(e: Expr): Expr = Div(this, e)
  def +(e: Expr): Expr = Add(this, e)
  def -(e: Expr): Expr = Sub(this, e)
  def unary_-   : Expr = Neg(this)

  def expr2Function2: ((Tuple2[Variable, Double], Tuple2[Variable, Double]) => SmartFloat) = {
    def evalapp(e: Expr): ((Tuple2[Variable, Double], Tuple2[Variable, Double]) => SmartFloat)
    = e match {
      case Zero => (x, y) => 0
      case Pow(a, b) => (x, y) => SmartFloat.pow(evalapp(a)(x, y), evalapp(b)(x, y))
      case Mul(a, b) => (x, y) => evalapp(a)(x, y) * evalapp(b)(x, y)
      case Div(a, b) => (x, y) => evalapp(a)(x, y) / evalapp(b)(x, y)
      case Add(a, b) => (x, y) => evalapp(a)(x, y) + evalapp(b)(x, y)
      case Sub(a, b) => (x, y) => evalapp(a)(x, y) - evalapp(b)(x, y)
      case Sin(a)    => (x, y) => SmartFloat.sin(evalapp(a)(x, y))
      case Cos(a)    => (x, y) => SmartFloat.cos(evalapp(a)(x, y))
      case Neg(a)    => (x, y) => - evalapp(a)(x, y)
      case v @ Variable(name) => (x, y) => {
        if (v == x._1)
          SmartFloat(x._2)
        else if (v == y._1)
          SmartFloat(y._2)
        else throw new CannotEvaluateException
      }
      case Const(c: SmartFloat) => (x, y) => c
      case FunctionVariable(_,_,_) => throw new CannotEvaluateException
      case d(u, x) => throw new CannotEvaluateException
      case dd(u, x, t) => throw new CannotEvaluateException
      case _ => throw new CannotEvaluateException
    }
    evalapp(this)
  }

  def eval(var1: Tuple2[Variable, Double], var2: Tuple2[Variable, Double]): SmartFloat = {
    def evalapp(e: Expr): ((SmartFloat, SmartFloat) => SmartFloat) = e match {
      case Zero => (x, y) => 0
      case Pow(a, b) => (x, y) => SmartFloat.pow(evalapp(a)(x, y), evalapp(b)(x, y))
      case Mul(a, b) => (x, y) => evalapp(a)(x, y) * evalapp(b)(x, y)
      case Div(a, b) => (x, y) => evalapp(a)(x, y) / evalapp(b)(x, y)
      case Add(a, b) => (x, y) => evalapp(a)(x, y) + evalapp(b)(x, y)
      case Sub(a, b) => (x, y) => evalapp(a)(x, y) - evalapp(b)(x, y)
      case Sin(a)    => (x, y) => SmartFloat.sin(evalapp(a)(x, y))
      case Cos(a)    => (x, y) => SmartFloat.cos(evalapp(a)(x, y))
      case Neg(a)    => (x, y) => - evalapp(a)(x, y)
      case v @ Variable(name) => (x, y) => {
        if (v == var1._1)
          SmartFloat(var1._2)
        else if (v == var2._1)
          SmartFloat(var2._2)
        else throw new CannotEvaluateException
      }
      case Const(c: SmartFloat) => (x, y) => c
      case FunctionVariable(_,_,_) => throw new CannotEvaluateException
      case d(u, x) => throw new CannotEvaluateException
      case dd(u, x, t) => throw new CannotEvaluateException
      case _ => throw new CannotEvaluateException
    }
    val fn = evalapp(this)
    fn(var1._2, var2._2)

    //TODO test speed of this function
  }

  def :=(e: Expr): PDE  = {
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
    def update(u: Function, e: Expr): Unit = e match {
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
        case Sub(a: Function, b)          => {
          update(a, Const(1.0)); splitUp(Neg(b))
        }
        case Sub(a, b)                    => {
          if (hasFunction(a)) {splitUp(a); splitUp(Neg(b))}
          else update(noFunction, Neg(Add(a, b)))
        }
        case Neg(a: Function)             => {
          update(a, Neg(Const(1)))
        }
        case Neg(Mul(b, a: Function))     => {
          update(a, Neg(b))
        }
        case Neg(Zero)                    => ();
        case Neg(a)                       => {
          if (hasFunction(a)) splitUp(a)
          else update(noFunction, Neg(a))
        }
        case Powf(f: Function, c)         => update(Powf(f, c), Const(1))
        case f: Function                  => {update(f, Const(1))}
        case Zero                         => ();
        case e: Expr                      => update(noFunction, e)
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
          case (a, Zero, Zero, h, b, c, Zero) => {
            h match {
              case Const(s) =>
                (a, b, c) match {
                  case (Neg(Const(e)), Neg(Const(f)), Neg(Const(g))) if e > 0 && h == 1.0 =>
                    new pde.model.ScalarHeatEquation(e, f, g, u)
                  case (Neg(Const(e)), Zero, Neg(Const(g))) if e > 0 && s == 1.0 =>
                    new pde.model.ScalarHeatEquation(e, SmartFloat(0.0), g, u)
                  case (Neg(Const(e)), Neg(Const(f)), Zero) if e > 0 && s == 1.0 =>
                    new pde.model.ScalarHeatEquation(e, f, SmartFloat(0.0), u)
                  case (Neg(Const(e)), Zero, Zero) if e >= 0 && s == 1.0 =>{
                    new pde.model.ScalarHeatEquation(e, SmartFloat(0.0), SmartFloat(0.0), u)
                  }
                  case _ => {new pde.model.HeatEquation(a, b, c, u)}
                }
              case Neg(Const(s)) =>
                (a, b, c) match {
                  case (Const(e),Const(f), Const(g)) if e > 0 && h == 1.0 =>
                    new pde.model.ScalarHeatEquation(e, f, g, u)
                  case (Const(e), Zero, Const(g)) if e > 0 && h == 1.0 =>
                    new pde.model.ScalarHeatEquation(e, SmartFloat(0.0), g, u)
                  case (Const(e), Const(f), Zero) if e > 0 && h == 1.0 =>
                    new pde.model.ScalarHeatEquation(e, f, SmartFloat(0.0), u)
                  case (Const(e), Zero, Zero) if e > 0 && h == 1.0 =>
                    new pde.model.ScalarHeatEquation(e, SmartFloat(0.0), SmartFloat(0.0), u)
                  case _ => {
                    new pde.model.HeatEquation(a, b, c, u)
                  }
                }

            }
          }
          case (a, Zero, b, Zero, Zero, Zero, Zero) => new pde.model.Laplace2(u)
          case (a, Zero, b, Zero, Zero, Zero, c) => new pde.model.Poisson2(u, c)
          case (Const(a), Const(b), Const(c), Const(d), Const(e), Const(f), Const(g)) =>
            new pde.model.ScalarLinear2(a, b, c, d, e, f, g, u)
          case _ => PDE.secondOrder(dtt, dxt, dxx, dt, dx, noOrder, nofunct, u)
        }
      }
    }
  }
}

object noFunction extends Function
case class Powf(f: Function, c: Expr) extends Expr with Function
case class Pow(a: Expr, b: Expr) extends Expr
case class Mul(a: Expr, b: Expr) extends Expr {
  override def toString() : String = {
    "(" + a + " * " + b + ")"
  }
}
case class Div(a: Expr, b: Expr) extends Expr {
  override def toString() : String = {
    "(" + a + " / " + b + ")"
  }
}
case class Add(a: Expr, b: Expr) extends Expr {
  override def toString() : String = {
    a + " + " + b
  }
}
case class Sub(a: Expr, b: Expr) extends Expr {
  override def toString() : String = {
    a + " - " + b
  }
}
case class Sin(a: Expr) extends Expr  {
  override def toString() : String = {
    "Sin(" + a + ")"
  }
}
case class Cos(a: Expr) extends Expr  {
  override def toString() : String = {
    "Cos(" + a + ")"
  }
}
case class Neg(a: Expr) extends Expr  {
  override def toString() : String = {
    "-(" + a + ")"
  }
}

class Const(val c : SmartFloat) extends Expr {
  def *(k: SmartFloat): Const = Const(c*k)
  def /(k: SmartFloat): Const = Const(c/k)
  def +(k: SmartFloat): Const = Const(c+k)
  def -(k: SmartFloat): Const = Const(c-k)

  override def toString() = c.d.toString
}

case object Zero extends Const(0.0) {
  override def toString(): String = "Zero"
  override def equals(other: Any): Boolean = other match {
    case x: Double => x == 0
    case x: Short => x  == 0
    case x: Char => x == 0
    case x: Byte => x == 0
    case x: Int => x == 0
    case x: Float => x == 0.0
    case x: Long => x == 0
    case Const(a) => a == 0.0
    case _ => false
  }
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



trait Function

abstract class Derivative extends Expr with Function
case class d(u: FunctionVariable, x: Variable) extends Derivative
case class dd(u: FunctionVariable, x: Variable, t: Variable) extends Derivative

object Variable {

  private var Uniquecounter = 1

}

class invalidVariableException(x: Variable) extends RuntimeException

sealed case class Variable(xname: String) extends Expr {
  override def toString(): String = xname
}


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

  override def toString(): String = uname
}

case class fixedVar(u: FunctionVariable, variable: Variable, c: Double) {
  // useage looks like u(0, t) :=6+5*t
  def :=(e: Expr): Condition = Condition(u, variable, c, e)
}

sealed abstract class Interval{
  val lo: Double
  val hi: Double
}

final class from(interval: scala.collection.immutable.Range.Inclusive) extends Interval {
  val lo: Double = interval.min
  val hi: Double = interval.max
  (assert(lo<hi))
}

object from{
  def apply(interval: scala.collection.immutable.Range.Inclusive): from = new from(interval)
}

final case class to(val lo: Double, val hi: Double) extends Interval {(assert(lo<hi))}
case class Condition(function: FunctionVariable, fixed: Variable, c: Double, exp: Expr)
abstract class Point
case class Point2(var1: Tuple2[Variable, Double], var2: Tuple2[Variable, Double]){
  val x = var1._1
  val xVal = var1._2
  val t = var2._1
  val tVal = var2._2
  val coordx = (x, xVal)
  val coordt = (t, tVal)
  def apply(x: Variable): Double = {
    if (x == var1._1) var1._2
    else if (x == var2._1) var2._2
    else throw new CannotEvaluateException
  }
}


case class BFunction(u: Condition, interval: from) {

  def lowerPoint: Point2 = if (u.fixed == u.function.x)
    Point2((u.function.x,  u.c),(u.function.t, interval.lo))
  else Point2((u.function.x, interval.lo),(u.function.t, u.c))
  def upperPoint: Point2 = if (u.fixed == u.function.x)
    Point2((u.function.x, u.c), (u.function.t, interval.hi))
  else Point2((u.function.x, interval.hi),(u.function.t, u.c))

  def lowerValue: SmartFloat = if (u.fixed == u.function.x)
    u.exp.eval((u.function.x, u.c), (u.function.t, interval.lo))
  else u.exp.eval((u.function.x, interval.lo), (u.function.t, u.c))
  def upperValue: SmartFloat = if (u.fixed == u.function.x)
    u.exp.eval((u.function.x,  u.c),(u.function.t, interval.hi))
  else u.exp.eval((u.function.x, interval.hi),(u.function.t, u.c))
}

object Expr{

  implicit def double2Const(c: Double) = Const(SmartFloat(c))
    implicit def ciT2BFunction(bf: Tuple2[Condition, from]): BFunction =
    BFunction(bf._1, bf._2)


}
