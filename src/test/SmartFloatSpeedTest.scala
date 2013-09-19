object SmartFloatSpeedTest extends App{
  import ceres.smartfloat.SmartFloat
  import pde.model.RectBoundary
  import pde.model.expression.{FunctionVariable, from, Variable, Neg, Const}
  import pde.solver.Solver.{generateFunctionVals, generateFunctionValsD, generateRectBorder, generateRectBorderD}
  import pde.model.expression.Expr.{ciT2BFunction}
  
  val t: Variable  = new Variable("t")
  val x: Variable = new Variable("x")
  val u: FunctionVariable = new FunctionVariable("u", x, t)
  val boundary = new RectBoundary(
    (u(0, t) := 3*t, from(0 to 10)),
    (u(10, t):= 20+3*t, from(0 to 10)),
    (u(x, 0) := 2*x, from(0 to 10)),
    (u(x, 10):= 2*x+30, from(0 to 10))
  )
  val size: Int = 101
  val step: Double = 0.1
  val solutionSF: Array[Array[SmartFloat]] = generateRectBorder(boundary, step, step, size, size)
  val solutionD: Array[Array[Double]] = generateRectBorderD(boundary, step, step, size, size)
  
  val dtValsSF : Array[Array[SmartFloat]] = generateFunctionVals(t, u, step, step, size, size, 0, 0)
  val dtValsD : Array[Array[Double]] = generateFunctionValsD(t, u, step, step, size, size, 0, 0)
  val dxValsSF : Array[Array[SmartFloat]] = generateFunctionVals(x, u, step, step, size, size, 0, 0)
  val dxValsD : Array[Array[Double]] = generateFunctionValsD(x, u, step, step, size, size, 0, 0)
  val noOrderValsSF : Array[Array[SmartFloat]] = generateFunctionVals(Neg(Const(1.0)),
                                                                      u, step, step, size, size, 0, 0)
  val noOrderValsD : Array[Array[Double]] = generateFunctionValsD(Neg(Const(1.0)),
                                                                  u, step, step, size, size, 0, 0)
  val noFunctValsSF : Array[Array[SmartFloat]] = Array.fill(size, size)(SmartFloat(0.0))
  val noFunctValsD : Array[Array[Double]] = Array.fill(size, size)(0.0)
  val quotientSF = {
    for (i <- 0 until size)
    yield 
      for(j <- 0 until size)
      yield 1/(dtValsSF(i)(j)/step + dxValsSF(i)(j)/step + noOrderValsSF(i)(j))
  }
  val quotientD = {
    for (i <- 0 until size)
    yield 
      for(j <- 0 until size)
      yield (dtValsD(i)(j)/step + dxValsD(i)(j)/step + noOrderValsD(i)(j))
  }
  val temp = Array.fill(size, size)(0.0)
  val temp2 = Array.fill(size, size)(SmartFloat(0.0))
  
  def uij(i: Int)(j: Int): Double = {
    if (quotientD(i)(j) != 0.0) {
      val centralValue = (dtValsD(i)(j)/step * solutionD(i)(j-1) +
                          dxValsD(i)(j)/step * solutionD(i-1)(j) -
                          noFunctValsD(i)(j)) /quotientD(i)(j)
      
      
      
      centralValue
    }
    else 0.0
  }

  def uijSF(i: Int)(j: Int): SmartFloat = {
    if (quotientSF(i)(j) != 0.0) {
      val centralValue = (dtValsSF(i)(j)/step * solutionSF(i)(j-1) +
                          dxValsSF(i)(j)/step * solutionSF(i-1)(j) -
                          noFunctValsSF(i)(j))*quotientSF(i)(j)

      val xError = - dxValsSF(i)(j) * ((step / 2) *
                                       (solutionSF(i+1)(j) - 2 * solutionSF(i)(j)
                                        + solutionSF(i-1)(j)))
      
      val tError = - dtValsSF(i)(j) * ((step / 2) *
                                       (solutionSF(i)(j+1) - 2 * solutionSF(i)(j)
                                        + solutionSF(i)(j-1)))
      
      
      centralValue addError xError addError tError
    }
    else SmartFloat(0.0)
  }
  def time[A](f: => A) = {
    val s = System.nanoTime
    val ret = f
    println("time: "+(System.nanoTime-s)/1e6+"ms")
    ret
  }
  time{
    for{i <- 1 until size-1;
        j <- 1 until size-1}{
          temp2(i)(j) = uijSF(i)(j)
        }
  }
  time{
    for{i <- 1 until size-1;
        j <- 1 until size-1}{
          temp(i)(j) = uij(i)(j)
        }
  }
  
}
