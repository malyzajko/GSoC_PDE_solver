//TBD
package pde.model

import pde.model.expression.{Function, Expr, FunctionVariable, dd, d, noFunction, Zero, Const, Add, Neg}
import ceres.smartfloat.{SmartFloat}
import SmartFloat.double2SmartFloat

sealed abstract class PDE {
  val function: Function
}
  
case class NotAPDEException()        extends Exception

object PDE {

  def firstOrder(map: scala.collection.immutable.Map[Function, Expr]): PDE1 = PDE1(map)
  def secondOrder(dtt: Expr, dxt: Expr, dxx: Expr,
                  dt: Expr, dx: Expr, noOrder: Expr, noFunct: Expr, f: FunctionVariable) =
                    new LinearPDE2(dtt, dxt, dxx, dt, dx, noOrder, noFunct, f)
  def secondOrder(map: scala.collection.immutable.Map[Function, Expr]) : PDE2
  = PDE2(map)

}

// First Order PDEs 


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
  
  override def toString() : String = {
    val dtString = dt match {
      case Zero => ""
      case Neg(Const(c)) =>
        if (c == 0.0) "" else   dt + " * " + function + "_t" 
      case Const(c) =>
        if (c.d == 0.0) "" else dt + " * " + function + "_t" 
      case a => "(" + dt + ") * " + function + "_t"
      }
      val dxString = dx match {
        case Zero => ""
        case Neg(Const(c)) =>
          if (c == 0.0) "" else   dx + " * " + function + "_x" 
        case Const(c) =>
          if (c == 0.0) "" else   dx + " * " + function + "_x"
        case a => "(" + dx + ") * " +function+ "_x" 
      }
      val noOrderString = noOrder match {
        case Zero  => ""
        case Const(c) => 
          if (c.d == 0.0) "" else noOrder + " * " + function
        case a => "(" + noOrder + ") * " + function
      }
      val noFunctString = noFunct match {
        case Zero => ""
        case Const(c) => ""
        case a => noFunct
      }
      val all = Array[String](dtString, dxString, noOrderString).dropWhile(str => str == "")
      var result = all.head
      for(str <- all.tail; if str != ""){
        result ++= " + " + str
      }
      if(noFunctString != "") result ++= " + " + noFunctString
      
      result + " = 0"
      
    }
  }

  // Second Order PDEs 

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
        Add(map(dd(u, u.x, u.t)), map(dd(u, u.t, u.x)))
                else if (map.contains(dd(u, u.x, u.t))) map(dd(u, u.x, u.t))
                else if (map.contains(dd(u, u.t, u.x))) map(dd(u, u.t, u.x))
                else Zero
      val dtx = if (map.contains( dd(u, u.x, u.t) ) ) map(dd(u, u.x, u.t)) else Zero
      val dxx = if (map.contains(dd(u, u.x, u.x) )) map(dd(u, u.x, u.x)) else Zero
      val dtt = if (map.contains(dd(u, u.t, u.t) )) map(dd(u, u.t, u.t)) else Zero
      (dxx, dxt, dtt, dt, dx, noOrder, nofunct) match {
        case (a, Zero, b, Zero, Zero, Zero, Zero) =>
          if (a == Const(1.0) && b == Const(1.0)) new Laplace2(u)
          else PDE.secondOrder(imap)
        case (Const(a), Const(b), Const(c), Const(d), Const(e), Const(f), Const(g)) =>
          if (c*c < 4*a*b) new ScalarElliptical(a, b, c, d, e, f, g, u)
          else if (c*c == 4*a*b) new ScalarParabolic(a, b, c, d, e, f, g, u)
          else new ScalarHyperbolic(a, b, c, d, e, f, g, u)
        case _ => PDE.secondOrder(dtt, dxt, dxx, dt, dx, noOrder, nofunct, u)
      }
      
    }

  }

  class LinearPDE2(val dtt: Expr, val dxt: Expr, val dxx: Expr,
                   val dt: Expr, val dx: Expr, val noOrder: Expr, val noFunct: Expr, f: FunctionVariable) extends PDE2 {
    
    val function = f
    
    override def toString() : String = {
      val dttString = dtt match {
        case Zero => ""
        case Const(c) =>
          if (c.d == 0.0) "" else dtt + " * " + f + "_tt" 
        case a => "(" + dtt + ") * " + f + "_tt"
      }
      val dxtString = dxt match {
        case Zero => ""
        case Const(c) =>
          if (c.d == 0.0) "" else  dxt + " * " + f + "_xt" 
        case a => "(" + dxt + ") * " + f + "_xt" 
      }
      val dxxString = dxx match {
        case Zero => ""
        case Const(c) =>
          if (c.d == 0.0) "" else dxx + " * " + f + "_xx" 
        case a => "(" + dxx + ") * " + f + "_xx" 
      }
      val dtString = dt match {
        case Zero => ""
        case Neg(Const(c)) =>
          if (c == 0.0) "" else   dt + " * " + f + "_t" 
        case Const(c) =>
          if (c.d == 0.0) "" else dt + " * " + f + "_t" 
        case a => "(" + dt + ") * " + f + "_t"
      }
      val dxString = dx match {
        case Zero => ""
        case Neg(Const(c)) =>
          if (c == 0.0) "" else   dx + " * " + f + "_x" 
        case Const(c) =>
          if (c == 0.0) "" else   dx + " * " + f + "_x" 
        case a => "(" + dx + ") * " + f + "_x" 
      }
      val noOrderString = noOrder match {
        case Zero  => ""
        case Const(c) => 
          if (c.d == 0.0) "" else noOrder + " * " + f
        case a => "(" + noOrder + ") * " + f
      }
      val noFunctString = noFunct match {
        case Zero => ""
        case Const(c) => ""
        case a => " - " + noFunct
      }
      val all = Array[String](dttString, dxtString, dxxString,
                              dtString, dxString, noOrderString).dropWhile(str => str == "")
      var result = all.head
      for(str <- all.tail; if str != ""){
        result ++= " + " + str
      }
      if(noFunctString != "") result ++= " - " + noFunctString
      
      result + " = 0"
        
    } 
  }

  class ScalarLinear2(dtt: SmartFloat, dxt: SmartFloat,  dxx: SmartFloat,
                      dt: SmartFloat,  dx: SmartFloat,  noOrder: SmartFloat,  noFunct: SmartFloat,  function: FunctionVariable)
  extends LinearPDE2(Const(dtt), Const(dxt),
                     Const(dxx), Const(dt), Const(dx), Const(noOrder), Const(noFunct), function){
                       val dttVal = dtt
                       val dxtVal = dxt
                       val dxxVal = dxx
                       val dtVal = dt
                       val dxVal = dx
                       val noOrderVal = noOrder
                       val noFunctVal = noFunct
                     }

class ScalarHeatEquation(dxx: SmartFloat, dx: SmartFloat, noOrder: SmartFloat, function: FunctionVariable)
extends LinearPDE2(Zero, Zero,
                   Const(dxx),Neg(Const(1.0)), Const(dx), Const(noOrder), Zero, function) {
                     val a = dxx
                     val b = dx
                     val c = noOrder
                   }

class HeatEquation(dxx: Expr, dx: Expr, noOrder: Expr, function: FunctionVariable)
extends LinearPDE2(Zero, Zero,
                   dxx,Neg(Const(1.0)), dx, noOrder, Zero, function)

class ScalarElliptical(dtt: SmartFloat, dxt: SmartFloat,  dxx: SmartFloat,
                       dt: SmartFloat,  dx: SmartFloat,  noOrder: SmartFloat,
                       noFunct: SmartFloat,  function: FunctionVariable)
extends LinearPDE2(Const(dtt), Const(dxt),
                   Const(dxx), Const(dt), Const(dx), Const(noOrder), Const(noFunct), function)

class ScalarParabolic(dtt: SmartFloat, dxt: SmartFloat,  dxx: SmartFloat,
                      dt: SmartFloat,  dx: SmartFloat,
                      noOrder: SmartFloat,  noFunct: SmartFloat,  function: FunctionVariable)
extends LinearPDE2(Const(dtt), Const(dxt),
                   Const(dxx), Const(dt), Const(dx), Const(noOrder), Const(noFunct), function)

class ScalarHyperbolic(dtt: SmartFloat, dxt: SmartFloat,  dxx: SmartFloat,
                       dt: SmartFloat,  dx: SmartFloat,
                       noOrder: SmartFloat,  noFunct: SmartFloat,  function: FunctionVariable)
extends LinearPDE2(Const(dtt), Const(dxt),
                   Const(dxx), Const(dt), Const(dx), Const(noOrder), Const(noFunct), function)

class Laplace2 (override val function: FunctionVariable)
extends LinearPDE2(Const(1), Zero, Const(1), Zero, Zero, Zero, Zero, function)

class Poisson2 (override val function: FunctionVariable, c: Expr)
extends LinearPDE2(Const(1), Zero, Const(1), Zero, Zero, Zero, c, function)
