/*/TBD
package pde.errorVal;
import pde.expression.{BFunction}
import pde.boundary.{RectBoundary}

class OutOfBoundsException extends Exception

class errorData(val data: Array[Array[ErrorVal]], boundary: RectBoundary){

  private val f = boundary.b1.u.function
  import boundary.{bottom, top, left, right}
  private val xmin = bottom.interval.lo
  private val xmax = bottom.interval.hi
  private val tmin = left.interval.lo
  private val tmax = left.interval.hi

  def apply(x: Double, t: Double): ErrorVal = {
    if (x < xmin || x > xmax || t < tmin || t > tmax) throw new OutOfBoundsException
    else if (x % 1.0 == 0.0 && t % 1.0 == 0.0) data(x.toInt)(t.toInt)
    else data(0)(0)
  }

}

sealed abstract class ErrorVal

object AffineDouble {

  private var counter = 0


  def apply(c: Double)(cs: Array[AffineCoefficient]): AffineDouble = new AffineDouble(c, cs)
  def unapply(x: AffineDouble) = Some((x.x0, x.coefficients))

  object AffineCoefficient {
    //reserved indices
    val const = -2
    val roundOff = -3
    val dx = -10
    val dt = -11

    def apply(c: Double, typ: String): AffineCoefficient = typ match {
      case "roundOff" => AffineCoefficient(c, roundOff)
      case "dx" => AffineCoefficient(c, dx)
      case "dt" => AffineCoefficient(c, dt)
      case _ => new AffineCoefficient(c)
    }

    def apply(c: Double, index: Int): AffineCoefficient = new AffineCoefficient(c, index)

    def apply(c: Double): AffineCoefficient = new AffineCoefficient(c)

  }

  sealed class AffineCoefficient(val deviation: Double, InputIndex: Int = -1){
    val index = if (InputIndex == -1) {
      val temp = AffineDouble.counter
      AffineDouble.counter += 1
      temp
    }
    else if (InputIndex > AffineDouble.counter){
      AffineDouble.counter = InputIndex + 1
      InputIndex
    }
    else InputIndex

    def *(c: Double) = AffineCoefficient(deviation*c, index)

    override def equals(other: Any) = other match {
      case that: AffineCoefficient => (index == that.index)
      case _ => false
    }
  }

  // def compact(coeffs: Array[AffineCoefficient]): Array[AffineCoefficient] = {
  //   def compactApp(ccs: Array[AffineCoefficient]): Array[AffineCoefficient] = {
  //   }
  //   val (smaller, greater) = coeffs.partition(a => a.value < Math.pow(2, -49))
  //   val compactedSmaller = compactApp(smaller)
  //   val avg = greater.sum/greater.length
  //   val std = Math.sqrt(greater.map(a => Math.pow(a-avg, 2)).sum)
  //   val (recompact, keep) = greater.partition(a => a.value < avg + std)
  //   val res2 = compactApp(recompact)
  // }

}

class AffineDouble(val x0: Double, val coefficients: Array[AffineDouble.AffineCoefficient])
    extends ErrorVal {

  import AffineDouble.{AffineCoefficient}

  def unary_- = AffineDouble(-this.x0)(
    this.coefficients.map(a => AffineCoefficient(-a.deviation, a.index)))

  private def containsCoeff(x: AffineDouble, coeff: AffineCoefficient) = {
    x.coefficients.find( (b: AffineCoefficient) => b.index == coeff.index ) match {
      case Some(a) => true
      case _ => false
    }
  }

  private def getCoefficient(cs: AffineDouble, c: AffineCoefficient) = cs match{
    case AffineDouble(x, xs) => xs(xs.indexOf(c, 0))
  }

  private def CoefficientOpp(x: AffineDouble,
    y: AffineDouble,
    fn: (AffineCoefficient, AffineCoefficient) => AffineCoefficient) = {

    val commonCoefficients = for(coeff <- x.coefficients
      if containsCoeff(y, coeff))
    yield fn(coeff, getCoefficient(y, coeff))

    val xOnly = for (coeff <- x.coefficients
      if !containsCoeff(y, coeff))
    yield coeff

    val yOnly = for (coeff <- y.coefficients
      if !containsCoeff(x, coeff))
    yield coeff

    (commonCoefficients ++ xOnly ++ yOnly).sortWith(
      (a: AffineCoefficient, b: AffineCoefficient) => a.index < b.index
    )
  }

  def +(b: AffineDouble) = {

    AffineDouble(this.x0 + b.x0)(
      CoefficientOpp(this, b,
        (a, b) => new AffineCoefficient((a.deviation+b.deviation), a.index)))
  }

  def -(b: AffineDouble) = AffineDouble(this.x0 - b.x0)(
    CoefficientOpp(this, b,
      (a, b) => new AffineCoefficient((a.deviation-b.deviation), a.index)))

  def *(that: AffineDouble) = {

    val commonCoefficients = for(coeff <- coefficients
      if containsCoeff(that, coeff))
    yield new AffineCoefficient(coeff.deviation * that.x0
      + getCoefficient(that, coeff).deviation * x0, coeff.index)

    val thisOnly = for (coeff <- this.coefficients
      if !containsCoeff(that, coeff))
    yield coeff * that.x0

    val thatOnly = for (coeff <- that.coefficients
      if !containsCoeff(this, coeff))
    yield coeff * x0

    val crossTerms = {
      val sumThis = this.coefficients.foldLeft(0.0)((a: Double, b: AffineCoefficient) => a + b.deviation.abs)
      val sumThat = that.coefficients.foldLeft(0.0)((a: Double, b: AffineCoefficient) => a + b.deviation.abs)
      new AffineCoefficient(sumThis*sumThat)
    }
    val temp = (commonCoefficients ++ thisOnly ++ thatOnly).+:(crossTerms)

    // if (temp.size >= 50) {
    //   val compacted = compact(coefficients)
    AffineDouble(this.x0*that.x0)(temp)
  }



  def +(c: Double) = AffineDouble(this.x0+c)(this.coefficients)
  def -(c: Double) = AffineDouble(this.x0-c)(this.coefficients)

  def *(c: Double) = AffineDouble(this.x0*c)(
    this.coefficients.map(a => AffineCoefficient(a.deviation*c, a.index)))

  def /(c: Double) = AffineDouble(this.x0/c)(
    this.coefficients.map(a => AffineCoefficient(a.deviation/c, a.index)))

  override def toString = {
    val sum = coefficients.foldLeft(0.0)((a: Double, b: AffineCoefficient) => a + b.deviation.abs)
    val sorted = coefficients.sortWith((a, b) => a.index < b.index)
    val min = x0 - sum
    val max = x0 + sum
    //val cfs = for (coeff <- sorted) yield "AffineCoefficient(" + coeff.deviation + "," +coeff.index + "))"

    "[" + min + ", " + max + "] " + x0
    //"AffineDouble("+x0 +")(Array(" + cfs.mkString(",") + "))"
  }
}


case class IntervalVal(min: Double, max: Double) extends ErrorVal{

  def +(that: IntervalVal) = IntervalVal(min + that.min, max + that.max)
  def -(that: IntervalVal) = IntervalVal(min - that.max, max - that.min)
  def *(that: IntervalVal) = IntervalVal(Array(min * that.min, min * that.max,
    max * that.min, max * that.max).min,
    Array(min * that.min, min * that.max, max * that.min, max * that.max).max)
  def /(that: IntervalVal) = if (that.min <= 0 && that.max >= 0) throw new Exception
  else this * IntervalVal(1/that.min, 1/that.max)

  def +(c: Double) = IntervalVal(min+c, max+c)
  def -(c: Double) = IntervalVal(min-c, max-c)
  def *(c: Double) = IntervalVal(min*c, max*c)
  def /(c: Double) = IntervalVal(min/c, max+c)

}
 */
