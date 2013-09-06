//TBD
package pde

import pde.expression.{Function, Expr, FunctionVariable, dd, d, noFunction, Zero, Const}

sealed abstract class PDE {
  val function: Function
}

case class NotAPDEException()        extends Exception

object PDE {
  import pde.expression.{Function, Expr}

  def firstOrder(map: scala.collection.immutable.Map[Function, Expr]): PDE1 = PDE1(map)
  def secondOrder(dtt: Expr, dxt: Expr, dxx: Expr,
    dt: Expr, dx: Expr, noOrder: Expr, noFunct: Expr, f: FunctionVariable) =
    new LinearPDE2(dtt, dxt, dxx, dt, dx, noOrder, noFunct, f)
  def secondOrder(map: scala.collection.immutable.Map[Function, Expr]) : PDE2
  = PDE2(map)

}

//////////////////////
// First Order PDEs //
//////////////////////

abstract class PDE1 extends PDE

object PDE1 {

  def apply(map: scala.collection.immutable.Map[Function, Expr]): LinearPDE1 = new LinearPDE1(map)

}

class LinearPDE1(val map: scala.collection.immutable.Map[Function, Expr]) extends PDE1 {
  val function = {
    def getFunction(kss: List[Function]): FunctionVariable = kss match {
      case (f: FunctionVariable)::ks => f
      case d(f, _)::ks               => f
      case dd(f, _, _)::ks           => f
      case k::ks                     => getFunction(ks)
      case Nil                       => throw new NotAPDEException
    }
    getFunction(map.keys.toList)
  }

  val noFunct = if (map.contains(noFunction)) map(noFunction) else Zero
  val noOrder = if (map.contains(function)) map(function) else Zero
  val dx = if (map.contains( d(function, function.x) ) ) map(d(function, function.x)) else Zero
  val dt = if (map.contains(d(function, function.t))) map(d(function, function.t)) else Zero
}

///////////////////////
// Second Order PDEs //
///////////////////////

abstract class PDE2 extends PDE

object PDE2 {

  def apply (map: scala.collection.immutable.Map[Function, Expr]) : PDE2 = {
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
    val nofunct = if (map.contains(noFunction)) map(noFunction) else Zero
    val noOrder = if (map.contains(u)) map(u) else Zero
    val dx = if (map.contains( d(u, u.x) ) ) map(d(u, u.x)) else Zero
    val dt = if (map.contains(d(u, u.t))) map(d(u, u.t)) else Zero
    val dxt = if (map.contains( dd(u, u.x, u.t) ) && map.contains(dd(u, u.x, u.t)))
      pde.expression.Add(map(dd(u, u.x, u.t)), map(dd(u, u.t, u.x)))
    else if (map.contains(dd(u, u.x, u.t))) map(dd(u, u.x, u.t))
    else if (map.contains(dd(u, u.t, u.x))) map(dd(u, u.t, u.x))
    else Zero
    val dtx = if (map.contains( dd(u, u.x, u.t) ) ) map(dd(u, u.x, u.t)) else Zero
    val dxx = if (map.contains(dd(u, u.x, u.x) )) map(dd(u, u.x, u.x)) else Zero
    val dtt = if (map.contains(dd(u, u.t, u.t) )) map(dd(u, u.t, u.t)) else Zero
      (dxx, dxt, dtt, dt, dx, noOrder, nofunct) match {
      case (a, Zero, b, Zero, Zero, Zero, Zero) =>
        if (a == Const(1.0) && b == Const(1.0)) new pde.Laplace2(u)
        else PDE.secondOrder(imap)
      case (Const(a), Const(b), Const(c), d, e, f, g) =>
        if (c*c < 4*a*b) new pde.Elliptical(Const(a), Const(b), Const(c), d, e, f, g, u)
        else if (c*c == 4*a*b) new pde.Parabolic(Const(a), Const(b), Const(c), d, e, f, g, u)
        else new pde.Hyperbolic(Const(a), Const(b), Const(c), d, e, f, g, u)
      case _ => PDE.secondOrder(dtt, dxt, dxx, dt, dx, noOrder, nofunct, u)
    }

  }

}

class LinearPDE2(val dtt: Expr, val dxt: Expr, val dxx: Expr,
  val dt: Expr, val dx: Expr, val noOrder: Expr, val noFunct: Expr, f: FunctionVariable) extends PDE2 {

  val function = f

  def toElliptical = new Elliptical(dtt, dxt, dxx, dt, dx, noOrder, noFunct, f)
  def toParabolic  = new Parabolic(dtt, dxt, dxx, dt, dx, noOrder, noFunct, f)
  def toHyperbolic = new Hyperbolic(dtt, dxt, dxx, dt, dx, noOrder, noFunct, f)

}

class Elliptical(dtt: Expr, dxt: Expr,  dxx: Expr,
  dt: Expr,  dx: Expr,  noOrder: Expr,  noFunct: Expr,  function: FunctionVariable)
    extends LinearPDE2(dtt, dxt, dxx, dt, dx, noOrder, noFunct, function)

class Parabolic( dtt: Expr,  dxt: Expr,  dxx: Expr,
  dt: Expr,  dx: Expr,  noOrder: Expr,  noFunct: Expr,  function: FunctionVariable)
    extends LinearPDE2(dtt, dxt, dxx, dt, dx, noOrder, noFunct, function)

class Hyperbolic( dtt: Expr,  dxt: Expr,  dxx: Expr,
  dt: Expr,  dx: Expr,  noOrder: Expr,  noFunct: Expr,  function: FunctionVariable)
    extends LinearPDE2(dtt, dxt, dxx, dt, dx, noOrder, noFunct, function)

class Laplace2 (override val function: FunctionVariable)
    extends LinearPDE2(Const(1), Zero, Const(1), Zero, Zero, Zero, Zero, function)

class Poisson2 (override val function: FunctionVariable, f: Expr)
    extends LinearPDE2(Const(1), Zero, Const(1), Zero, Zero, Zero, f, function)
