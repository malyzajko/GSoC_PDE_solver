//TBD
package pde
import pde.expression.{BFunction, Const}
import pde.boundary.{Boundary, RectBoundary}
import pde.errorVal.{ErrorVal, AffineVal}
import pde.errorVal.AffineVal.AffineCoefficient
import scala.collection.immutable.Map

object Solver {

  class NotImplementedException extends Exception

  def solve(eqn: PDE, boundary: Boundary): Array[Array[AffineVal]] = eqn match {
    case order1: PDE1 => solveOrder1(order1, boundary)
    case _ => throw new NotImplementedException
      //case order2: PDE2 => solveOrder2(order2, boundary)
  }

  private def solveOrder1(eqn: PDE1, boundary: Boundary) = (eqn, boundary) match {
    case (linear1: LinearPDE1, rb: RectBoundary) => solveRectangularBoundary1(linear1, rb)
    case _ => throw new NotImplementedException

  }

  private def solveRectangularBoundary1(eqn: LinearPDE1, boundary: RectBoundary) = {
    val f = eqn.function
    val (bottom: BFunction, top: BFunction, left: BFunction, right: BFunction) = {
      var sorted = (scala.collection.mutable.ArrayBuffer
        (boundary.b1, boundary.b2, boundary.b3, boundary.b4)).sortWith(
        (a, b) => a.lowerPoint(f.t) < b.lowerPoint(f.t))
      val bot = sorted.find ( b => f.t == b.u.fixed ) match {
        case Some(b) => b
        case _ => throw new Exception
      }
      val tp = sorted.last
      sorted -= (bot, tp)
      sorted = sorted.sortWith(
        (a, b) => a.lowerPoint(f.x) < b.lowerPoint(f.x))
      val lef = sorted.head
      val rig = sorted.last
      (bot, tp, lef, rig)
    }
    val xstep = (bottom.interval.b - bottom.interval.a)/ 100
    val tstep = (left.interval.b - left.interval.a) / 100
    val xsize = ((bottom.interval.b - bottom.interval.a) / xstep + 1).toInt
    val tsize = ((left.interval.b - left.interval.a) / tstep + 1).toInt
    val xmin  = bottom.interval.a
    val tmin  = left.interval.a

    var solution = Array.fill(xsize, tsize) (AffineVal(0.0)(Array(AffineCoefficient(0, "Const"))))
    var temp = Array.ofDim[AffineVal](xsize, tsize)


    // The functions preceding dx, dt, u and f evaluated at points
    var dtVals = Array.ofDim[AffineVal](xsize, tsize)
    var dxVals = Array.ofDim[AffineVal](xsize, tsize)
    var noOrderVals = Array.ofDim[AffineVal](xsize, tsize)
    var noFunctVals = Array.ofDim[AffineVal](xsize, tsize)

    for (i <- 0 until xsize){
      val xpoint = bottom.interval.a + i*xstep
      solution(i)(0) = bottom.u.exp.eval(Map(f.x -> xpoint, f.t -> bottom.u.c))
      solution(i)(tsize-1) = top.u.exp.eval(Map(f.x -> xpoint, f.t -> top.u.c))
    }
    for (i <- 0 until tsize) {
      val tpoint =left.interval.a + i*tstep
      solution(0)(i) = left.u.exp.eval(Map(f.x -> left.u.c, f.t -> tpoint))
      solution(xsize-1)(i) = right.u.exp.eval(Map(f.x -> right.u.c, f.t -> tpoint))
    }

    {
      import eqn.{dx, dt, noOrder, noFunct}
      for (i <- 0 until xsize){
        for (j <- 0 until tsize)
        {
          val xpoint = xmin + i * xstep
          val tpoint = tmin + j * tstep
          val evalPoint = Map(f.x -> xpoint, f.t -> tpoint)
          dxVals(i)(j) = dx.eval(evalPoint)/xstep
          dtVals(i)(j) = dt.eval(evalPoint)/tstep
          noOrderVals(i)(j) = noOrder.eval(evalPoint)
          noFunctVals(i)(j) = noFunct.eval(evalPoint)
        }
      }
    }

    var iterations = 0
    def generateSolutionApp{
      var diff = 0.0
      def uij(i: Int)(j: Int): AffineVal = {
        val evaluatedPoint =
          if ((dtVals(i)(j)/tstep + dxVals(i)(j)/xstep + noOrderVals(i)(j)).centralValue != 0.0) {
            (dtVals(i)(j) * solution(i)(j-1) +
              dxVals(i)(j) * solution(i-1)(j) -
              noFunctVals(i)(j)) /
            (dtVals(i)(j).centralValue
              + dxVals(i)(j).centralValue
              + noOrderVals(i)(j).centralValue)
          }
        //remove .central value from the denominator when division is implemented for affinevals
          else AffineVal(0)(Array(AffineCoefficient(0, "Const")))

        val xError = - dxVals(i)(j).centralValue * ((xstep / 2) *
          (solution(i+1)(j).centralValue - 2 * solution(i)(j).centralValue
            + solution(i-1)(j).centralValue))

        val tError = - dtVals(i)(j).centralValue * ((tstep / 2) *
          (solution(i)(j+1).centralValue - 2 * solution(i)(j).centralValue
            + solution(i)(j-1).centralValue))

        AffineVal(evaluatedPoint.centralValue)(evaluatedPoint.coefficients) + AffineVal(0.0)(
          Array(AffineCoefficient(xError, "dx"), AffineCoefficient(tError, "dt")))
      }

      for{i <- 1 until xsize - 1;
        j <- 1 until tsize - 1}{
        temp(i)(j) = uij(i)(j)
        val temp2 = ((temp(i)(j) - solution(i)(j)) / temp(i)(j).centralValue).centralValue.abs
        if (temp2 > diff) diff = temp2
      }

      /*
       println("dx: " + dxVals(1)(1) + "\ndt: "
       + dtVals(1)(1) + "\nnoFunct: "
       + noFunctVals(1)(1) + "\nnoOrder: " + noOrderVals(1)(1))
       */

      for (i <- 1 until xsize-1; j <- 1 until tsize-1)
        solution(i)(j) = temp(i)(j)

      if (diff > 0.0001 && iterations < 10){
        iterations += 1
        generateSolutionApp
      }

    }
    generateSolutionApp

    solution
  }
}
/*
 private def solveOrder2(eqn: PDE2, boundary: Boundary) = eqn match {
 case linear2: LinearPDE2 => {
 (linear2, boundary) match {
 case (laplace2: Laplace2, rb: RectBoundary) => solveLaplace2(laplace2, rb)
 case (poisson2: Poisson2, rb: RectBoundary) => solvePoisson2(poisson2, rb)
 case (_, rb: RectBoundary) => solveGeneralLinear2(linear2, rb)
 }
 }
 }

 private def solveLaplace2(eqn: Laplace2, boundary: RectBoundary) = {
 val f = eqn.function
 val (bottom: BFunction, top: BFunction, left: BFunction, right: BFunction) = {
 var sorted = (scala.collection.mutable.ArrayBuffer
 (boundary.b1, boundary.b2, boundary.b3, boundary.b4)).sortWith(
 (a, b) => a.lowerPoint(f.t) < b.lowerPoint(f.t))
 val bot = sorted.find ( b => f.t == b.u.fixed ) match {
 case Some(b) => b
 case _ => throw new Exception
 }
 val tp = sorted.last
 sorted -= (bot, tp)
 sorted = sorted.sortWith(
 (a, b) => a.lowerPoint(f.x) < b.lowerPoint(f.x))
 val lef = sorted.head
 val rig = sorted.last
 (bot, tp, lef, rig)
 }

 val xstep = (bottom.interval.b - bottom.interval.a)/ 100
 val tstep = (left.interval.b - left.interval.a) / 100
 val xsize = ((bottom.interval.b - bottom.interval.a) / xstep + 1).toInt
 val tsize = ((left.interval.b - left.interval.a) / tstep + 1).toInt
 val xmin  = bottom.interval.a
 val tmin  = left.interval.a



 var solution = Array.fill(xsize, tsize) (0.0)
 var temp = Array.fill(xsize, tsize) (0.0)
 var error = Array.fill(xsize, tsize) (0.0)

 for (i <- 0 until xsize){
 val xpoint = bottom.interval.a + i * xstep
 solution(i)(0) = bottom.u.exp.eval(Map(f.x -> xpoint, f.t -> bottom.u.c))
 solution(i)(tsize - 1) = top.u.exp.eval(Map(f.x -> xpoint, f.t -> top.u.c))
 }
 for (i <- 0 until tsize) {
 val tpoint =left.interval.a + i*tstep
 solution(0)(i) = left.u.exp.eval(Map(f.x -> left.u.c, f.t -> tpoint))
 solution(xsize - 1)(i) = right.u.exp.eval(Map(f.x -> right.u.c, f.t -> tpoint))
 }


 var iterations = 0
 def generateSolutionApp{
 var maxError = 0.0
 def uij(i: Int)(j: Int): Double = {
 ( (solution(i-1)(j) + solution(i+1)(j)) / (xstep * xstep)
 + (solution(i)(j-1) + solution(i)(j+1))/(tstep * tstep)) / (2 * ((xstep*xstep
 + tstep * tstep) / (xstep * xstep * tstep * tstep)))
 }
 for{i <- 1 until xsize-1;
 j <- 1 until tsize-1}{
 temp(i)(j) = uij(i)(j)
 error(i)(j) = ((temp(i)(j) - solution(i)(j))).abs
 if (error(i)(j) > maxError) maxError = error(i)(j)
 }
 for (i <- 1 until xsize-1; j <- 1 until tsize-1)
 solution(i)(j) = temp(i)(j)

 if (maxError > 0.0000001 && iterations < 100000){
 iterations += 1
 generateSolutionApp
 }

 }


 generateSolutionApp
 (solution, error)

 }

 private def solvePoisson2(eqn: Poisson2, boundary: RectBoundary) = {

 (Array(Array(0.0)),Array(Array(0.0)))
 }


 private def solveGeneralLinear2(eqn: LinearPDE2, boundary: RectBoundary) = {
 val f = eqn.u
 val (bottom: BFunction, top: BFunction, left: BFunction, right: BFunction) = {
 var sorted = (scala.collection.mutable.ArrayBuffer
 (boundary.b1, boundary.b2, boundary.b3, boundary.b4)).sortWith(
 (a, b) => a.lowerPoint(f.t) < b.lowerPoint(f.t))
 val bot = sorted.find ( b => f.t == b.u.fixed ) match {
 case Some(b) => b
 case _ => throw new Exception
 }
 val tp = sorted.last
 sorted -= (bot, tp)
 sorted = sorted.sortWith(
 (a, b) => a.lowerPoint(f.x) < b.lowerPoint(f.x))
 val lef = sorted.head
 val rig = sorted.last
 (bot, tp, lef, rig)
 }

 val xstep = (bottom.interval.b - bottom.interval.a)/ 100
 val tstep = (left.interval.b - left.interval.a) / 100
 val xsize = ((bottom.interval.b - bottom.interval.a) / xstep + 1).toInt
 val tsize = ((left.interval.b - left.interval.a) / tstep + 1).toInt
 val xmin  = bottom.interval.a
 val tmin  = left.interval.a



 var solution = Array.fill(xsize, tsize) (0.0)
 var temp = Array.fill(xsize, tsize) (0.0)
 var error = Array.fill(xsize, tsize) (0.0)

 for (i <- 0 until xsize){
 val xpoint = xmin + i * xstep
 solution(i)(0) = bottom.u.exp.eval(Map(f.x -> xpoint, f.t -> bottom.u.c))
 solution(i)(tsize-1) = top.u.exp.eval(Map(f.x -> xpoint, f.t -> top.u.c))
 }
 for (i <- 0 until tsize) {
 val tpoint = tmin + i * tstep
 solution(0)(i) = left.u.exp.eval(Map(f.x -> left.u.c, f.t -> tpoint))
 solution(xsize-1)(i) = right.u.exp.eval(Map(f.x -> right.u.c, f.t -> tpoint))
 }

 var iterations = 0
 def generateSolutionApp{
 var maxError = 0.0
 def uij(i: Int)(j: Int): Double = {
 import eqn.{dxx, dtt, dxt, dx, dt, noFunct, noOrder}
 val xpoint = xmin + i * xstep
 val tpoint = tmin + j * tstep
 val evalPoint = Map(f.x -> xpoint, f.t -> tpoint)
 (xstep * xstep * tstep * tstep) * (dxx.eval(evalPoint) * (solution(i-1)(j) + solution(i+1)(j)) / (xstep * xstep)
 + dtt.eval(evalPoint) * (solution(i)(j-1) + solution(i)(j+1)) / (tstep * tstep)
 + dxt.eval(evalPoint) * (solution(i+1)(j+1)
 - solution(i+1)(j-1) - solution(i-1)(j+1) + solution(i+1)(j-1)) / (4 * xstep * tstep)
 + dx.eval(evalPoint) * (solution(i+1)(j)) / xstep
 + dt.eval(evalPoint) * (solution(i)(j+1)) / tstep
 + noFunct.eval(evalPoint)) /
 (2 * tstep * tstep * dxx.eval(evalPoint)
 + 2 * xstep * xstep * dtt.eval(evalPoint)
 + xstep * tstep * tstep * dx.eval(evalPoint)
 + xstep * xstep * tstep * dt.eval(evalPoint)
 + xstep * xstep * tstep * tstep * noOrder.eval(evalPoint))
 }
 for{i <- 1 until xsize-1;
 j <- 1 until tsize-1}{
 temp(i)(j) = uij(i)(j)
 error(i)(j) = ((temp(i)(j) - solution(i)(j))/temp(i)(j)).abs
 if (error(i)(j) > maxError) maxError = error(i)(j)
 }
 for (i <- 1 until xsize-1; j <- 1 until tsize-1)
 solution(i)(j) = temp(i)(j)


 if (maxError > 0.000000001 && iterations < 10000){
 iterations += 1
 generateSolutionApp
   }

   }


   generateSolutionApp
 (solution, error)

 }


 }
 */
