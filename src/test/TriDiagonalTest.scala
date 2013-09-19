import org.scalatest._
import ceres.smartfloat.SmartFloat
import pde.util.TriDiagonalMatrix

object TriDiagonalTest extends App {

  val twoD = TriDiagonalMatrix(Vector[SmartFloat](2, 5),
                               Vector[SmartFloat](3), Vector[SmartFloat](-1))
  val twoDSolution = twoD.solveSystem(Array[SmartFloat](-8, 1))
  
  twoDSolution.foreach(x => print(x.d+", "))
  println("")
  
  val unit = Vector[SmartFloat](1, 1, 1)
  val unitOffDiag = Vector[SmartFloat](0, 0)
  val I = TriDiagonalMatrix(unit, unitOffDiag, unitOffDiag)
  val testUnit = I.solveSystem(Array[SmartFloat](1, 1, 1))
  
  testUnit.foreach(x => print(x.d + ", "))
  println("")
  
  
  val mainD = Vector[SmartFloat](1, -3, 4)
  val upper = Vector[SmartFloat](2, 4)
  val lower = Vector[SmartFloat](6, 5)
  val d = Array[SmartFloat](17, 52, 70)
  val other = TriDiagonalMatrix(mainD, upper, lower)
  val testOther = other.solveSystem(d)
  testOther.foreach(x => print(x.d + ", "))
  println("")
  
  val test4 = Vector[SmartFloat](2, 4, 3, 2)
  val upper4 = Vector[SmartFloat](1, 5, 4)
  val lower4 = Vector[SmartFloat](-3, -2, 1)
  val test4m = TriDiagonalMatrix(test4, upper4, lower4)
  val test4s = test4m.solveSystem(Array[SmartFloat](8, 4 , -5, -1))
  test4s.foreach(x => print(x.d + ", "))
  println("")
  val main = Vector[SmartFloat](10.25, 10.25)
  val others = Vector[SmartFloat](-5)

  val aMatrix = TriDiagonalMatrix(main, others, others)
  val n1 = aMatrix.solveSystem(Vector[SmartFloat](-13.487, -40.2867))
  n1.foreach(x => print(x.d + ", "))
  println("")

}
