//TBD
package pde.errorVal;
import pde.expression.{BFunction}
import pde.boundary.{RectBoundary}

class OutOfBoundsException extends Exception

class errorData(val data: Array[Array[ErrorVal]], boundary: RectBoundary){

  private val f = boundary.b1.u.function
  private val (bottom: BFunction, top: BFunction, left: BFunction, right: BFunction) = {
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

  private val xmin = bottom.interval.a
  private val xmax = bottom.interval.b
  private val tmin = left.interval.a
  private val tmax = left.interval.b

  def apply(x: Double, t: Double): ErrorVal = {
    if (x < xmin || x > xmax || t < tmin || t > tmax) throw new OutOfBoundsException
    else if (x % 1.0 == 0.0 && t % 1.0 == 0.0) data(x.toInt)(t.toInt)
    else data(0)(0)
  }

}

sealed abstract class ErrorVal

object AffineVal {

  private var counter = 0


  def apply(c: Double)(cs: Array[AffineCoefficient]): AffineVal = new AffineVal(c, cs)

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
  }

  sealed class AffineCoefficient(val value: Double, InputIndex: Int = -1){
    val index = if (InputIndex == -1) {
      val temp = AffineVal.counter
      AffineVal.counter += 1
      temp
    }
    else InputIndex

    def *(c: Double) = AffineCoefficient(value*c, index)

    override def equals(other: Any) = other match {
      case that: AffineCoefficient => (index == that.index)
      case _ => false
    }
  }

}

class AffineVal(val centralValue: Double, val coefficients: Array[AffineVal.AffineCoefficient])
    extends ErrorVal {

  import AffineVal.{AffineCoefficient}

  def unary_- = AffineVal(-this.centralValue)(
    this.coefficients.map(a => AffineCoefficient(-a.value, a.index)))

  private def containsCoeff(coeffs: Array[AffineCoefficient], coeff: AffineCoefficient) = {
    coeffs.find( (b: AffineCoefficient) => b.index == coeff.index ) match {
      case Some(a) => true
      case _ => false
    }
  }

  private def getCoefficient(cs: Array[AffineCoefficient], c: AffineCoefficient) = {
    cs(cs.indexOf(c, 0))
  }

  private def CoefficientOpp(x: Array[AffineCoefficient],
    y: Array[AffineCoefficient],
    fn: (AffineCoefficient, AffineCoefficient) => AffineCoefficient) = {

    val commonCoefficients = for(coeff <- x
      if containsCoeff(y, coeff))
    yield fn(coeff, getCoefficient(y, coeff))

    val xOnly = for (coeff <- x
      if !containsCoeff(y, coeff))
    yield coeff

    val yOnly = for (coeff <- y
      if !containsCoeff(x, coeff))
    yield coeff

    (commonCoefficients ++ xOnly ++ yOnly).sortWith(
      (a: AffineCoefficient, b: AffineCoefficient) => a.index < b.index
    )
  }

  def +(b: AffineVal) = AffineVal(this.centralValue + b.centralValue)(
    CoefficientOpp(coefficients, b.coefficients,
      (a, b) => new AffineCoefficient((a.value+b.value), a.index)))

  def -(b: AffineVal) = AffineVal(this.centralValue - b.centralValue)(
    CoefficientOpp(coefficients, b.coefficients,
      (a, b) => new AffineCoefficient((a.value-b.value), a.index)))

  def *(that: AffineVal) = {

    val commonCoefficients = for (coeff <- this.coefficients
      if containsCoeff(that.coefficients, coeff))
    yield new AffineCoefficient(centralValue*getCoefficient(that.coefficients, coeff).value
      + that.centralValue*coeff.value, coeff.index)

    val thisOnly = for (coeff <- this.coefficients
      if !containsCoeff(that.coefficients, coeff))
    yield coeff

    val thatOnly = for (coeff <- that.coefficients
      if !containsCoeff(this.coefficients, coeff))
    yield coeff

    val crossTerms = {
      val sumThis = this.coefficients.foldLeft(0.0)((a: Double, b: AffineCoefficient) => a + b.value.abs)
      val sumThat = that.coefficients.foldLeft(0.0)((a: Double, b: AffineCoefficient) => a + b.value.abs)
      new AffineCoefficient(sumThis*sumThat)
    }
    val temp = commonCoefficients ++ thisOnly ++ thatOnly
    AffineVal(this.centralValue*that.centralValue)((temp.+:(crossTerms)).sortWith(
      (a: AffineCoefficient, b: AffineCoefficient) => a.index < b.index))
  }

  def +(c: Double) = AffineVal(this.centralValue+c)(this.coefficients)
  def -(c: Double) = AffineVal(this.centralValue-c)(this.coefficients)

  def *(c: Double) = AffineVal(this.centralValue*c)(
    this.coefficients.map(a => AffineCoefficient(a.value*c, a.index)))

  def /(c: Double) = AffineVal(this.centralValue/c)(
    this.coefficients.map(a => AffineCoefficient(a.value/c, a.index)))

  override def toString = {
    val sum = coefficients.foldLeft(0.0)((a: Double, b: AffineCoefficient) => a + b.value.abs)
    val min = centralValue - sum
    val max = centralValue + sum

    "[" + min + ", " + max + "] " + centralValue
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

}
