package pde

import pde.expression._
class PDE2 extends PDE

object PDE2 {

  def apply (map: scala.collection.mutable.Map[Function, Expr]) : PDE =
    new LinearPDE2(map)

}

class LinearPDE2(map: scala.collection.mutable.Map[Function, Expr]) extends PDE2 {
  val u = {
    def getFunction(kss: List[Function]): FunctionVariable = kss match {
      case (f: FunctionVariable)::ks => f
      case d(f, _)::ks               => f
      case dd(f, _, _)::ks           => f
      case k::ks                     => getFunction(ks)
    }
    getFunction(map.keys.toList)
  }

  val noFunct = if (map.contains(noFunction)) map(noFunction) else Zero
  val noOrder = if (map.contains(u)) map(u) else Zero
  val dx = if (map.contains( d(u, u.x) ) ) map(d(u, u.x)) else Zero
  val dt = if (map.contains(d(u, u.t))) map(d(u, u.t)) else Zero
  val dxt = if (map.contains( dd(u, u.x, u.t) ) ) map(dd(u, u.x, u.t)) else Zero
  val dtx = if (map.contains( dd(u, u.x, u.t) ) ) map(dd(u, u.x, u.t)) else Zero
  val dxx = if (map.contains(dd(u, u.x, u.x) )) map(dd(u, u.x, u.x)) else Zero
  val dtt = if (map.contains(dd(u, u.t, u.t) )) map(dd(u, u.t, u.t)) else Zero
}

case class Laplace2 (function: FunctionVariable)
    extends LinearPDE2(scala.collection.mutable.Map(dd(function, function.x, function.x) -> Const(1),
      dd(function, function.t, function.t) -> Const(1))){

}
