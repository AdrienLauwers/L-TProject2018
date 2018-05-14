package example

import example.ScalaJSExample.{Op, lineMatrixOp, matrix}

import scala.collection.mutable.ArrayBuffer

class Matrix(var elements:ArrayBuffer[ArrayBuffer[Double]] = new ArrayBuffer[ArrayBuffer[Double]]()) {

  def addLine(i : Array[Double]) = {
    elements += i.to[ArrayBuffer]
  }
  def addColumn(i : Array[Double]) = {
    for((x,j) <- elements.view.zipWithIndex){elements(j)+= i(j)}
  }
  def resetData() = {
    elements = new ArrayBuffer[ArrayBuffer[Double]]()
  }

  def apply(y:Int, x:Int) = data(y)(x)

  def update(y:Int, x:Int, value:Double) = data(y)(x) = value

  def map(f:Double => Double) = {
    new Matrix(data.map(_.map(f).to[ArrayBuffer]).to[ArrayBuffer])
  }

  def combine(rhs:Matrix, f:(Double, Double) => Double) = {
    var res = matrix.empty(lineLength,columnLength)
    for (y <- 0 until this.lineLength; x <- 0 until this.columnLength) {
      val l = this(y, x)
      val r = rhs(y, x)
      res(y, x) += f(l,r)
    }
    res
  }

  def +(number:Double) = map(_ + number)

  def *(number:Double) = map(_ * number)

  def /(number:Double) = map(_ / number)

  def +(other:Matrix) = combine(other, _ + _)

  def *(other:Matrix) = {
    val res = matrix.empty(lineLength,columnLength)
    for(y <- 0 until lineLength; x <- 0 until other.columnLength) {
      res(y, x) = (0 until columnLength).foldLeft(0.0) {
        case (sum, i) => sum + (this(y, i) * other(i, x))
      }
    }
    res
  }


  def /(other:Matrix) = {
    val res = matrix.empty(lineLength,columnLength)
    for(y <- 0 until lineLength; x <- 0 until other.columnLength) {
      res(y, x) = (0 until columnLength).foldLeft(0.0) {
        case (sum, i) => sum + (this(y, i) / other(i, x))
      }
    }
    res
  }

  def having(op: Op*) : Matrix = {
    op.foreach(_ match {
      case lineMatrixOp(i) => {
        elements += i.to[ArrayBuffer]
      }
    })
    return this;
  }
  def lineLength = data.length
  def columnLength = data(0).length
  def data = elements
}