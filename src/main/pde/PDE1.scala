package pde

import pde.expression._

class PDE1 extends PDE 

object PDE1 {
  
  def apply (map: scala.collection.mutable.Map[Function, Expr]) = new LinearPDE1(map)
  
}

class LinearPDE1(val map: scala.collection.mutable.Map[Function, Expr]) extends PDE1 {
  val function = {
    def getFunction(kss: List[Function]): FunctionVariable = kss match {
      case (f: FunctionVariable)::ks => f
      case d(f, _)::ks               => f
      case dd(f, _, _)::ks           => f
      case k::ks                     => getFunction(ks)
    }
    getFunction(map.keys.toList)
  }

  val noFunct = if (map.contains(noFunction)) map(noFunction) else Zero
  val noOrder = if (map.contains(function)) map(function) else Zero
  val dx = if (map.contains( d(function, function.x) ) ) map(d(function, function.x)) else Zero
  val dt = if (map.contains(d(function, function.t))) map(d(function, function.t)) else Zero
}
