package example

import example.ScalaJSExample._

import scala.collection.mutable.ArrayBuffer

/** The class matrix allows to create and make operations on matricies **/
class Matrix(var elements:ArrayBuffer[ArrayBuffer[Double]] = new ArrayBuffer[ArrayBuffer[Double]]()) {
  /** Add a line to the matrix **/
  def addLine(i : Array[Double]) = {
    elements += i.to[ArrayBuffer]
  }
  /** Add a column to the matrix **/
  def addColumn(i : Array[Double]) = {
    for((x,j) <- elements.view.zipWithIndex){elements(j)+= i(j)}
  }
  /** Reset all the data contained in the matrix **/
  def resetData() = {
    elements = new ArrayBuffer[ArrayBuffer[Double]]()
  }

  /** Get a specific element from the matrix **/
  def apply(y:Int, x:Int) = data(y)(x)

  /** Update a specific element of the matrix **/
  def update(y:Int, x:Int, value:Double) = data(y)(x) = value

  /** Make operation on the matrix **/
  def map(f:Double => Double) = {
    new Matrix(data.map(_.map(f).to[ArrayBuffer]).to[ArrayBuffer])
  }

  /** Make operation on two matricies **/
  def combine(rhs:Matrix, f:(Double, Double) => Double) = {
    var res = matrix.empty(lineLength,columnLength)
    for (y <- 0 until this.lineLength; x <- 0 until this.columnLength) {
      val l = this(y, x)
      val r = rhs(y, x)
      res(y, x) += f(l,r)
    }
    res
  }

  /** Add a number to each element of the matrix **/
  def +(number:Double) = map(_ + number)

  /** Multiply each ealement of the matrix by a number **/
  def *(number:Double) = map(_ * number)

  /** Divides each ealement of the matrix by a number **/
  def /(number:Double) = map(_ / number)

  /** Sum two matricies **/
  def +(other:Matrix) = combine(other, _ + _)

  /** Multiply two matricies **/
  def *(other:Matrix) = {
    val res = matrix.empty(lineLength,columnLength)
    for(y <- 0 until lineLength; x <- 0 until other.columnLength) {
      res(y, x) = (0 until columnLength).foldLeft(0.0) {
        case (sum, i) => sum + (this(y, i) * other(i, x))
      }
    }
    res
  }

  /** Divide two matricies **/
  def /(other:Matrix) = {
    val res = matrix.empty(lineLength,columnLength)
    for(y <- 0 until lineLength; x <- 0 until other.columnLength) {
      res(y, x) = (0 until columnLength).foldLeft(0.0) {
        case (sum, i) => sum + (this(y, i) / other(i, x))
      }
    }
    res
  }

  /** used in the DSL to construct the matrix **/
  def having(op: Op*) : Matrix = {
    op.foreach(_ match {
      case lineMatrixOp(i) => {
        elements += i.to[ArrayBuffer]
      }
    })
    return this;
  }

  /** Display the matrix **/
  def displayMatrix() = "[" + data.map {_.foldLeft("")(_ + " " + _.toString)}.reduceLeft(_ + "\n" + _) + "]"

  /** Number of lines in the matrix **/
  def lineLength = data.length
  /** Number of columns in the matrix **/
  def columnLength = data(0).length
  /** Elements in the matrix **/
  def data = elements
}