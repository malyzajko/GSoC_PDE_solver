package pde.util;

import ceres.smartfloat.SmartFloat
import collection.IndexedSeqLike
import collection.mutable.{Builder, ArrayBuffer}
import collection.generic.CanBuildFrom
import ceres.smartfloat.{SmartFloat}


class MatrixIndexOutOfBoundsException extends java.lang.ArrayIndexOutOfBoundsException
class DataAlreadyPresentException extends Exception
class noSolutionException extends Exception

object SparseMatrix {

  def apply[T](data: Vector[Vector[T]]): SparseMatrix = new SparseMatrix[T](data)
  def zeroes: SparseMatrix = new SparseMatrix[SmartFloat](Vector(Vector()))

}

class SparseMatrix[T](data: Vector[Vector[T]]) {
  assert(data.length == data(0).length, println("Not a square matrix"))

  val length = data.length

  private val rows = {
    val tempRows = scala.collection.mutable.Map[Int, SparseVector[T]]()

    for(i <- 0 until length){
      for (j <- 0 until length){
        val tempRow = SparseVector[T](length)
        if (data(i)(j) != 0){
          tempRow += (j, data(i)(j))
        }
        if (!tempRow.isEmpty) tempRows += ((i, tempRow))
      }
    }
    tempRows
  }

  def +=(i: Int, j: Int, data: T) {
    if (i < 0 || i > length) throw new MatrixIndexOutOfBoundsException
    else if (j < 0 || j > length) throw new MatrixIndexOutOfBoundsException
    else if (rows(i)(j) != 0) throw new DataAlreadyPresentException
    else rows(i) += (j, data)
  }

  def +=(i: Int, vec: SparseVector[T]){
    rows += ((i, vec))
  }

  def apply(i: Int, j: Int): T = {
    rows(i)(j)

  }
}

object TriDiagonalMatrix{

  def apply(main: Vector[SmartFloat],
    upper: Vector[SmartFloat], lower: Vector[SmartFloat]): TriDiagonalMatrix =
    new TriDiagonalMatrix(main, upper, lower)
}

class TriDiagonalMatrix(val main: Vector[SmartFloat],
  val upper: Vector[SmartFloat],
  val lower: Vector[SmartFloat]){
  assert(main.length -1 == upper.length && main.length-1 == lower.length)

  def determinant: SmartFloat = {
    val fneg1 = 0
    val f0 =1
    val f1: SmartFloat = main(0)
    val f2 = main(1)*f1-upper(0)*lower(0)*f0
    var f = List[SmartFloat](main(1)*f1-upper(0)*lower(0), main(0))
    for(i <- 1 until main.length){
      f = (main(i)*f.head-lower(i-1)*upper(i-1)*f.tail.head)::f
    }
    f.head
  }

  def solveSystem(ds: IndexedSeq[SmartFloat]): IndexedSeq[SmartFloat] = {
    val n = ds.length
    if (n == 0) throw new noSolutionException
    else if (n == 1) Array(ds(0))
    else if (n==2) {
      if (main(0)*main(1) - lower(0)*upper(0) == 0) throw new noSolutionException
      else {
        /**
          * ( a  b ) (x) = (e)
          * ( c  d ) (y)   (f)
          */
        val y = (lower.head * ds.head
          - main.head * ds.last) / (upper.head*lower.head
            - main.head*main.last)
        val x = (main.last * ds.head
          - upper.head * ds.last) / (main.head * main.last
            - upper.head * lower.head)
        Array(x, y)
      }
    }
    else {
      val c = upper.map(elem => elem.d).toArray
      val a = lower.map(elem => elem.d)
      val b = main.map(elem => elem.d).toArray
      val d = ds.map(elem => elem.d).toArray

      for(i <- 0 until n-1){
        d(i+1) -= d(i) * a(i)/b(i)
        b(i+1) -= c(i) * a(i)/b(i)
      }
      for(i <- n-2 to 0 by -1)
        d(i) -= d(i+1) * c(i)/b(i+1)

      for(i <- 0 until n)
      yield SmartFloat(d(i)/b(i))
    }
  }
}

object SparseVector{

  def apply[T](length: Int): SparseVector[T] = new SparseVector[T](length)

}

class SparseVector[T](vallength: Int){

  private val sparseRow = scala.collection.mutable.Map[Int, T]()

  def +=(j: Int, value: T){
    if (value != 0) sparseRow += ((j, value))
  }

  def isEmpty: Boolean = sparseRow.isEmpty

  def apply(j: Int): T = sparseRow(j)
}
