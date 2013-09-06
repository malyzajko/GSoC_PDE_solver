//TBD

// For the reference, a first order PDE is
// a du/dt + b du/dx + c u + f = 0
package pde
import pde.expression._
import pde.boundary._
import ceres.smartfloat.{SmartFloat}

object Solver {

  def solve(eqn: PDE, boundary: Boundary,
    xstep: Double = 0.01, tstep: Double = 0.01): Array[Array[SmartFloat]] = eqn match {
    case order1: PDE1 => solveOrder1(order1, boundary)(xstep, tstep)
    case order2: PDE2 => solveOrder2(order2, boundary)(xstep, tstep)
  }

  private def solveOrder1(eqn: PDE1, boundary: Boundary)
    (xstep: Double = 0.01, tstep: Double = 0.01) = (eqn, boundary) match {
    case (linear1: LinearPDE1, rb: RectBoundary) =>
      solveRectangularBoundary1(linear1, rb)(xstep, tstep)
    case _ => throw new CannotEvaluateException
  }

  private def solveRectangularBoundary1(eqn: LinearPDE1, boundary: RectBoundary)
    (xstep: Double = 0.01, tstep: Double = 0.01) = {
    import scala.collection.immutable.Map
    import eqn.{function => f}
    import boundary.{bottom, top, left, right}

    val xsize = ((bottom.interval.hi - bottom.interval.lo) / xstep + 1).toInt
    val tsize = ((left.interval.hi - left.interval.lo) / tstep + 1).toInt
    val xmin  = bottom.interval.lo
    val tmin  = left.interval.lo

    var temp = Array.ofDim[SmartFloat](xsize, tsize)

    // Boundary Values
    var solution = generateRectBorder(boundary,xstep, tstep, xsize, tsize)

    // The functions preceding dx, dt, u and f, so a, b, c and d
    // evaluated at points in the area
    val dtVals = generateFunctionVals(eqn.dt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxVals = generateFunctionVals(eqn.dx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noOrderVals = generateFunctionVals(eqn.noOrder, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noFunctVals = generateFunctionVals(eqn.noFunct, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val quotientZero = {
      for (i <- 0 until xsize)
      yield {
        for (j <- 0 until tsize)
        yield ((dtVals(i)(j)/tstep + dxVals(i)(j)/xstep + noOrderVals(i)(j)) == 0.0)
      }
    }

    var iterations = 0
    def generateSolutionApp() {
      var diff = 0.0
      def uij(i: Int)(j: Int): SmartFloat = {
        if (!quotientZero(i)(j)) {
          val centralValue = (dtVals(i)(j)/tstep * solution(i)(j-1) +
            dxVals(i)(j)/xstep * solution(i-1)(j) -
            noFunctVals(i)(j)) /
          (dtVals(i)(j)/tstep + dxVals(i)(j)/xstep + noOrderVals(i)(j))

          val xError = - dxVals(i)(j) * ((xstep / 2) *
            (solution(i+1)(j) - 2 * solution(i)(j)
              + solution(i-1)(j)))

          val tError = - dtVals(i)(j) * ((tstep / 2) *
            (solution(i)(j+1) - 2 * solution(i)(j)
              + solution(i)(j-1)))

          centralValue
        }
        else SmartFloat(0.0)
      }

      for{i <- 1 until xsize-1;
        j <- 1 until tsize-1}{
        temp(i)(j) = uij(i)(j)
        val temp2 = SmartFloat.abs((temp(i)(j) - solution(i)(j)) / temp(i)(j))
        if (temp2 > diff) {diff = temp2.d}
      }

      for (i <- 1 until xsize-1; j <- 1 until tsize-1)
        solution(i)(j) = temp(i)(j)


      // println("iters " +iterations +", point " + solution(1)(1))

      if (diff > 0.0001 && iterations < 1000){
        iterations += 1
        generateSolutionApp
      }

    }
    generateSolutionApp()

    solution

  }

  //////////////////////
  // Order 2 Solution //
  //////////////////////


  private def solveOrder2(eqn: PDE2, boundary: Boundary)
    (xstep: Double = 0.01, tstep: Double = 0.01) = eqn match {
    case linear2: LinearPDE2 => (linear2, boundary) match {
      case (laplace2: Laplace2, rb: RectBoundary) => solveLaplace2(laplace2, rb)(xstep,tstep)
      case (ell: Elliptical, tsb: ThreeSidedBoundary) => solveElliptical(ell, tsb)(xstep, tstep)
      case (par: Parabolic, rb: RectBoundary) => solveParabolic(par, rb)(xstep, tstep)
      case (hyp: Hyperbolic, rb: RectBoundary) => solveHyperbolic(hyp, rb)(xstep, tstep)
      case (_, rb: RectBoundary) =>{

        def classifyPDE2(a: Expr, b: Expr, c: Expr, bdf: RectBoundary) = {
          val evaluatedPoints =
            for (i <- bdf.bottom.lowerPoint(bdf.f.x) to bdf.bottom.upperPoint(bdf.f.x) by xstep;
              j <- bdf.left.lowerPoint(bdf.f.t) to bdf.left.upperPoint(bdf.f.t) by tstep;
              evalPoint = Map(bdf.f.x -> i, bdf.f.t -> j);
              aval = a.eval(evalPoint);
              bval = b.eval(evalPoint)/2;
              cval = c.eval(evalPoint))
            yield bval*bval - 4*aval*cval

          if (evaluatedPoints.forall(a => a == 0)) PARA
          else if (evaluatedPoints.forall(a => a < 0)) ELLI
          else if (evaluatedPoints.forall(a => a > 0)) HYPE
          else GENE
        }

        case object ELLI
        case object PARA
        case object HYPE
        case object GENE

        classifyPDE2(linear2.dxx, linear2.dxt, linear2.dtt, rb) match {
          case ELLI => solveEllipticalR(linear2.toElliptical, rb)(xstep, tstep)
          case PARA => solveParabolic(linear2.toParabolic, rb)(xstep, tstep)
          case HYPE => solveHyperbolic(linear2.toHyperbolic, rb)(xstep, tstep)
          case GENE =>  solveGeneralLinear2CrankNickl(linear2, rb)(xstep, tstep)

        }
      }
    }
  }

  private def solveLaplace2(eqn: Laplace2, boundary: RectBoundary)
    (xstep: Double = 0.01, tstep: Double = 0.01) = {
    import scala.collection.immutable.Map
    import eqn.{function => f}
    import boundary.{bottom, top, left, right}

    val xsize = ((bottom.interval.hi - bottom.interval.lo) / xstep + 1).toInt
    val tsize = ((left.interval.hi - left.interval.lo) / tstep + 1).toInt
    val xmin  = bottom.interval.lo
    val tmin  = left.interval.lo

    var temp = Array.ofDim[SmartFloat](xsize, tsize)
    var solution = generateRectBorder(boundary,xstep, tstep, xsize, tsize)

    var iterations = 0
    def generateSolutionApp(){
      var diff = 0.0
      def uij(i: Int)(j: Int) = {
        ((solution(i-1)(j) + solution(i+1)(j))/(xstep*xstep)
          +(solution(i)(j-1) + solution(i)(j+1))/(tstep*tstep))/(2*((xstep*xstep
            +tstep*tstep)/(xstep*xstep*tstep*tstep)))
      }
      for{i <- 1 until xsize-1;
        j <- 1 until tsize-1}{
        temp(i)(j) = uij(i)(j)
        val rDiff = SmartFloat.abs((temp(i)(j) - solution(i)(j))/temp(i)(j))
        if (rDiff > diff) diff = rDiff.d
      }

      for (i <- 1 until xsize-1; j <- 1 until tsize-1)
        solution(i)(j) = temp(i)(j)

      if (diff > 0.00001 && iterations < 100000){
        iterations += 1
        generateSolutionApp()
      }

    }


    generateSolutionApp()

    solution
  }

  private def solveEllipticalR(eqn: Elliptical, boundary: RectBoundary)
    (xstep: Double = 0.01, tstep: Double = 0.01) = {
    import scala.collection.immutable.Map
    import eqn.{function => f}
    import boundary.{bottom, top, left, right}
    val xsize = ((bottom.interval.hi - bottom.interval.lo) / xstep + 1).toInt
    val tsize = ((left.interval.hi - left.interval.lo) / tstep + 1).toInt
    val xmin  = bottom.interval.lo
    val tmin  = left.interval.lo

    var temp = Array.ofDim[SmartFloat](xsize, tsize)

    // Inputting the Boundary Values
    var solution = generateRectBorder(boundary,xstep, tstep, xsize, tsize)
    val dttVals = generateFunctionVals(eqn.dtt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxtVals = generateFunctionVals(eqn.dxt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxxVals = generateFunctionVals(eqn.dxx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dtVals = generateFunctionVals(eqn.dt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxVals = generateFunctionVals(eqn.dx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noOrderVals = generateFunctionVals(eqn.noOrder, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noFunctVals = generateFunctionVals(eqn.noFunct, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val quotientZero = {
      for (i <- 0 until xsize)
      yield {
        for (j <- 0 until tsize)
        yield ((dtVals(i)(j)/tstep + dxVals(i)(j)/xstep + noOrderVals(i)(j)) == 0.0)
      }
    }
    solution
  }

  private def solveElliptical(eqn: Elliptical, boundary: ThreeSidedBoundary)
    (xstep: Double = 0.01, tstep: Double = 0.01) = {
    import scala.collection.immutable.Map
    import eqn.{function => f}
    import boundary.{bottom, left, right}
    val xsize = ((bottom.interval.hi - bottom.interval.lo) / xstep + 1).toInt
    val tsize = 1000
    val xmin  = bottom.interval.lo
    val tmin  = bottom.u.c

    var temp = Array.ofDim[SmartFloat](xsize, tsize)

    // Inputting the Boundary Values
    var solution = generateThreeSidedBorder(boundary,xstep, tstep, xsize, tsize)
    val dttVals = generateFunctionVals(eqn.dtt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxtVals = generateFunctionVals(eqn.dxt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxxVals = generateFunctionVals(eqn.dxx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dtVals = generateFunctionVals(eqn.dt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxVals = generateFunctionVals(eqn.dx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noOrderVals = generateFunctionVals(eqn.noOrder, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noFunctVals = generateFunctionVals(eqn.noFunct, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val quotientZero = {
      for (i <- 0 until xsize)
      yield {
        for (j <- 0 until tsize)
        yield ((dtVals(i)(j)/tstep + dxVals(i)(j)/xstep + noOrderVals(i)(j)) == 0.0)
      }
    }
    solution
  }

  private def solveParabolic(eqn: Parabolic, boundary: RectBoundary)
    (xstep: Double = 0.01, tstep: Double = 0.01) = {
    import scala.collection.immutable.Map
    import eqn.{function => f}
    import boundary.{bottom, left, right}
    val xsize = ((bottom.interval.hi - bottom.interval.lo) / xstep + 1).toInt
    val tsize = 1000
    val xmin  = bottom.interval.lo
    val tmin  = bottom.u.c

    var temp = Array.ofDim[SmartFloat](xsize, tsize)

    // Inputting the Boundary Values
    var solution = generateRectBorder(boundary,xstep, tstep, xsize, tsize)
    val dttVals = generateFunctionVals(eqn.dtt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxtVals = generateFunctionVals(eqn.dxt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxxVals = generateFunctionVals(eqn.dxx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dtVals = generateFunctionVals(eqn.dt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxVals = generateFunctionVals(eqn.dx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noOrderVals = generateFunctionVals(eqn.noOrder, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noFunctVals = generateFunctionVals(eqn.noFunct, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val quotientZero = {
      for (i <- 0 until xsize)
      yield {
        for (j <- 0 until tsize)
        yield ((dtVals(i)(j)/tstep + dxVals(i)(j)/xstep + noOrderVals(i)(j)) == 0.0)
      }
    }
    solution
  }

  private def solveHyperbolic(eqn: Hyperbolic, boundary: RectBoundary)
    (xstep: Double = 0.01, tstep: Double = 0.01)= {
    import scala.collection.immutable.Map
    import eqn.{function => f}
    import boundary.{bottom, top, left, right}
    val xsize = ((bottom.interval.hi - bottom.interval.lo) / xstep + 1).toInt
    val tsize = ((left.interval.hi - left.interval.lo) / tstep + 1).toInt
    val xmin  = bottom.interval.lo
    val tmin  = left.interval.lo

    var temp = Array.ofDim[SmartFloat](xsize, tsize)

    // Inputting the Boundary Values
    var solution = generateRectBorder(boundary,xstep, tstep, xsize, tsize)

    val dttVals = generateFunctionVals(eqn.dtt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxtVals = generateFunctionVals(eqn.dxt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxxVals = generateFunctionVals(eqn.dxx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dtVals = generateFunctionVals(eqn.dt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxVals = generateFunctionVals(eqn.dx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noOrderVals = generateFunctionVals(eqn.noOrder, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noFunctVals = generateFunctionVals(eqn.noFunct, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val quotientZero = {
      for (i <- 0 until xsize)
      yield {
        for (j <- 0 until tsize)
        yield ((dtVals(i)(j)/tstep + dxVals(i)(j)/xstep + noOrderVals(i)(j)) == 0.0)
      }
    }

    solution
  }



  private def solveGeneralLinear2CrankNickl(eqn: LinearPDE2, boundary: RectBoundary)
    (xstep: Double = 0.01, tstep: Double = 0.01)= {
    import scala.collection.immutable.Map
    import eqn.{function => f}
    import boundary.{bottom, top, left, right}
    val xsize = ((bottom.interval.hi - bottom.interval.lo) / xstep + 1).toInt
    val tsize = ((left.interval.hi - left.interval.lo) / tstep + 1).toInt
    val xmin  = bottom.interval.lo
    val tmin  = left.interval.lo

    var temp = Array.ofDim[SmartFloat](xsize, tsize)

    // Inputting the Boundary Values
    var solution = generateRectBorder(boundary,xstep, tstep, xsize, tsize)

    // The functions preceding dx, dt, u and f, so a, b, c and d
    // evaluated at points in the area
    val dttVals = generateFunctionVals(eqn.dtt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxtVals = generateFunctionVals(eqn.dxt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxxVals = generateFunctionVals(eqn.dxx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dtVals = generateFunctionVals(eqn.dt, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val dxVals = generateFunctionVals(eqn.dx, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noOrderVals = generateFunctionVals(eqn.noOrder, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val noFunctVals = generateFunctionVals(eqn.noFunct, f, xstep, tstep, xsize, tsize, xmin, tmin)
    val quotientZero = {
      for (i <- 0 until xsize)
      yield {
        for (j <- 0 until tsize)
        yield ((dtVals(i)(j)/tstep + dxVals(i)(j)/xstep + noOrderVals(i)(j)) == 0.0)
      }
    }

    var iterations = 0
    val xstepSquared = xstep * xstep
    val tstepSquared = tstep * tstep
    def generateSolutionApp() {
      var diff = 0.0
      def uij(i: Int)(j: Int) = {
        import eqn._
        val xpoint = xmin+i*xstep
        val tpoint = tmin+j*tstep
        val evalPoint = Map(f.x -> xpoint, f.t -> tpoint)
        (xstepSquared*tstepSquared)*(dxxVals(i)(j)*(solution(i-1)(j)+solution(i+1)(j))/(xstepSquared)
          + dttVals(i)(j)*(solution(i)(j-1)+solution(i)(j+1))/(tstepSquared)
          +dxtVals(i)(j)*(solution(i+1)(j+1)
            - solution(i+1)(j-1) - solution(i-1)(j+1)+solution(i+1)(j-1))/(4*xstep*tstep)
          +dxVals(i)(j)*(solution(i+1)(j))/xstep
          +dtVals(i)(j)*(solution(i)(j+1))/tstep
          +noFunctVals(i)(j))/
        (2*tstepSquared*dxxVals(i)(j)
          +2*xstepSquared*dttVals(i)(j)
          +xstep*tstepSquared*dxVals(i)(j)
          +xstepSquared*tstep*dtVals(i)(j)
          +xstepSquared*tstepSquared*noOrderVals(i)(j))
      }
      for{i <- 1 until xsize-1;
        j <- 1 until tsize-1}{
        temp(i)(j) = uij(i)(j)
        val temp2 = SmartFloat.abs((temp(i)(j) - solution(i)(j)) / temp(i)(j))
        if (temp2 > diff) {diff = temp2.d}
      }
      for (i <- 1 until xsize-1; j <- 1 until tsize-1)
        solution(i)(j) = temp(i)(j)


      if (diff > 0.000000001 && iterations < 10000){
        iterations += 1
        generateSolutionApp
      }

    }

    generateSolutionApp()
    solution

  }


  private def generateRectBorder(boundary: RectBoundary, xstep: Double, tstep: Double,
    xsize: Int, tsize: Int): Array[Array[SmartFloat]] = {
    import boundary.{bottom, top, left, right, f}
    var solution = Array.ofDim[SmartFloat](xsize, tsize)
    for (i <- 0 until xsize){
      val xpoint = bottom.interval.lo + i*xstep
      val tpoint = left.interval.lo + i*tstep
      solution(i)(0) = bottom.u.exp.eval(Map(f.x -> xpoint, f.t -> bottom.u.c))
      solution(i)(tsize-1) = top.u.exp.eval(Map(f.x -> xpoint, f.t -> top.u.c))
      solution(0)(i) = left.u.exp.eval(Map(f.x -> left.u.c, f.t -> tpoint))
      solution(xsize-1)(i) = right.u.exp.eval(Map(f.x -> right.u.c, f.t -> tpoint))
    }

    solution
  }

  private def generateThreeSidedBorder(boundary: ThreeSidedBoundary, xstep: Double, tstep: Double,
    xsize: Int, tsize: Int): Array[Array[SmartFloat]] = {
    import boundary.{bottom, left, right, f}
    var solution = Array.ofDim[SmartFloat](xsize, tsize)
    for (i <- 0 until xsize){
      val xpoint = bottom.interval.lo + i*xstep
      val tpoint = bottom.u.c + i*tstep
      solution(i)(0) = bottom.u.exp.eval(Map(f.x -> xpoint, f.t -> bottom.u.c))
      solution(0)(i) = left.exp.eval(Map(f.x -> left.c, f.t -> tpoint))
      solution(xsize-1)(i) = right.exp.eval(Map(f.x -> right.c, f.t -> tpoint))
    }

    solution
  }

  private def generateFunctionVals(e: Expr, f: FunctionVariable,
    xstep: Double, tstep: Double, xsize: Int, tsize: Int,
    xmin: Double, tmin: Double): Array[Array[SmartFloat]] = e match {
    case Zero     => Array.fill(xsize,tsize)(SmartFloat(0.0))
    case Const(c) => Array.fill(xsize,tsize)(c)
    case _        => {
      val result = Array.ofDim[SmartFloat](xsize,tsize)
      for (i <- 0 until xsize) {
        for (j <- 0 until tsize) {
          val xpoint = xmin + i * xstep
          val tpoint = tmin + j * tstep
          val evalPoint = Map(f.x -> xpoint, f.t -> tpoint)
          result(i)(j) = e.eval(evalPoint)
        }
      }

      result
    }
  }
}
