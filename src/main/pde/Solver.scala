package pde
import pde._
import pde.expression._
import pde.boundary._

object Solver {

  def solve(eqn: PDE, boundary: Boundary): Tuple2[Array[Array[Double]], Array[Array[Double]]] = eqn match {
    case order1: PDE1 => solveOrder1(order1, boundary)
    case order2: PDE2 => solveOrder2(order2, boundary)
  }
  
  private def solveOrder1(eqn: PDE1, boundary: Boundary) = (eqn, boundary) match {
    case (linear1: LinearPDE1, rb: RectBoundary) => solveRectangularBoundary1(linear1, rb)
  }

  private def solveRectangularBoundary1(eqn: LinearPDE1, boundary: RectBoundary) = {
    import scala.collection.immutable.Map
    val f = eqn.function
    val (bottom: BFunction, top: BFunction, left: BFunction, right: BFunction) = {
      var sorted = (scala.collection.mutable.ArrayBuffer
        (boundary.b1, boundary.b2, boundary.b3, boundary.b4)).sortWith(
        (a, b) => a.lowerPoint(f.t) < b.lowerPoint(f.t))
      val bot = sorted.find ( b => f.t == b.fixed.variable ) match {
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
      val xpoint = bottom.interval.a + i*xstep
      solution(i)(0) = bottom.u.exp.eval(Map(f.x -> xpoint, f.t -> bottom.fixed.c))
      solution(i)(tsize-1) = top.u.exp.eval(Map(f.x -> xpoint, f.t -> top.fixed.c))
    }
    for (i <- 0 until tsize) {
      val tpoint =left.interval.a + i*tstep
      solution(0)(i) = left.u.exp.eval(Map(f.x -> left.fixed.c, f.t -> tpoint))
      solution(xsize-1)(i) = right.u.exp.eval(Map(f.x -> right.fixed.c, f.t -> tpoint))
    }
    var iterations = 0
    def generateSolutionApp{
      var maxError = 0.0
      def uij(i: Int)(j: Int) = {
        import eqn._
        val xpoint = xmin+i*xstep
        val tpoint = tmin+j*tstep
        val evalPoint = Map(f.x -> xpoint, f.t -> tpoint)
        val xval = dx.eval(evalPoint)/xstep
        val tval = dt.eval(evalPoint)/tstep
        if (dx.eval(evalPoint) != 0)
          (tval*solution(i)(j-1)+xval*solution(i-1)(j) + noFunct.eval(evalPoint))/(
            tval+xval+noOrder.eval(evalPoint))
        else 0
      }
      for{i <- 1 until xsize-1;
        j <- 1 until tsize-1}{
        temp(i)(j) = uij(i)(j)
        if (temp(i)(j) !=0) {
          error(i)(j) = ((temp(i)(j) - solution(i)(j))/temp(i)(j)).abs
          if (error(i)(j) > maxError) maxError = error(i)(j)
        }
      }

      for (i <- 1 until xsize-1; j <- 1 until tsize-1)
        solution(i)(j) = temp(i)(j)
      
      
      // println("iters " +iterations +", point " + solution(1)(1))

      if (maxError > 0.0001 && iterations < 1000){
        iterations += 1
        generateSolutionApp
      }

    }
    generateSolutionApp
    println(iterations)
    (solution, error)
    
  }
  

  private def solveOrder2(eqn: PDE2, boundary: Boundary) = eqn match {
    case linear2: LinearPDE2 => (linear2, boundary) match {
      case (laplace2: Laplace2, rb: RectBoundary) => solveLaplace2(laplace2, rb)
      case (_, rb: RectBoundary) => solveGeneralLinear2(linear2, rb)
    }
  }

  private def solveLaplace2(eqn: Laplace2, boundary: RectBoundary) = {
    import scala.collection.immutable.Map
    val f = eqn.function
    val (bottom: BFunction, top: BFunction, left: BFunction, right: BFunction) = {
      var sorted = (scala.collection.mutable.ArrayBuffer
        (boundary.b1, boundary.b2, boundary.b3, boundary.b4)).sortWith(
        (a, b) => a.lowerPoint(f.t) < b.lowerPoint(f.t))
      val bot = sorted.find ( b => f.t == b.fixed.variable ) match {
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
      val xpoint = bottom.interval.a + i*xstep
      solution(i)(0) = bottom.u.exp.eval(Map(f.x -> xpoint, f.t -> bottom.fixed.c))
      solution(i)(tsize-1) = top.u.exp.eval(Map(f.x -> xpoint, f.t -> top.fixed.c))
    }
    for (i <- 0 until tsize) {
      val tpoint =left.interval.a + i*tstep
      solution(0)(i) = left.u.exp.eval(Map(f.x -> left.fixed.c, f.t -> tpoint))
      solution(xsize-1)(i) = right.u.exp.eval(Map(f.x -> right.fixed.c, f.t -> tpoint))
    }

    
    var iterations = 0
    def generateSolutionApp{
      var maxError = 0.0
      def uij(i: Int)(j: Int) = {
        ( (solution(i-1)(j) + solution(i+1)(j))/(xstep*xstep)
          +(solution(i)(j-1) + solution(i)(j+1))/(tstep*tstep))/(2*((xstep*xstep
            +tstep*tstep)/(xstep*xstep*tstep*tstep)))
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


  private def solveGeneralLinear2(eqn: LinearPDE2, boundary: RectBoundary) = {
    import scala.collection.immutable.Map
    val f = eqn.u
    val (bottom: BFunction, top: BFunction, left: BFunction, right: BFunction) = {
      var sorted = (scala.collection.mutable.ArrayBuffer
        (boundary.b1, boundary.b2, boundary.b3, boundary.b4)).sortWith(
        (a, b) => a.lowerPoint(f.t) < b.lowerPoint(f.t))
      val bot = sorted.find ( b => f.t == b.fixed.variable ) match {
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
      val xpoint = xmin + i*xstep
      solution(i)(0) = bottom.u.exp.eval(Map(f.x -> xpoint, f.t -> bottom.fixed.c))
      solution(i)(tsize-1) = top.u.exp.eval(Map(f.x -> xpoint, f.t -> top.fixed.c))
    }
    for (i <- 0 until tsize) {
      val tpoint = tmin + i*tstep
      solution(0)(i) = left.u.exp.eval(Map(f.x -> left.fixed.c, f.t -> tpoint))
      solution(xsize-1)(i) = right.u.exp.eval(Map(f.x -> right.fixed.c, f.t -> tpoint))
    }
    
    var iterations = 0
    def generateSolutionApp{
      var maxError = 0.0
      def uij(i: Int)(j: Int) = {
        import eqn._
        val xpoint = xmin+i*xstep
        val tpoint = tmin+j*tstep
        val evalPoint = Map(f.x -> xpoint, f.t -> tpoint)
          (xstep*xstep*tstep*tstep)*(dxx.eval(evalPoint)*(solution(i-1)(j)+solution(i+1)(j))/(xstep*xstep)
            + dtt.eval(evalPoint)*(solution(i)(j-1)+solution(i)(j+1))/(tstep*tstep)
            +dxt.eval(evalPoint)*(solution(i+1)(j+1)
              - solution(i+1)(j-1) - solution(i-1)(j+1)+solution(i+1)(j-1))/(4*xstep*tstep)
            +dx.eval(evalPoint)*(solution(i+1)(j))/xstep
            +dt.eval(evalPoint)*(solution(i)(j+1))/tstep
            +noFunct.eval(evalPoint))/
        (2*tstep*tstep*dxx.eval(evalPoint)
          +2*xstep*xstep*dtt.eval(evalPoint)
          +xstep*tstep*tstep*dx.eval(evalPoint)
          +xstep*xstep*tstep*dt.eval(evalPoint)
          +xstep*xstep*tstep*tstep*noOrder.eval(evalPoint))
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
