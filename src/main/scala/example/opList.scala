package example

trait Op
case class colorOp(i: String) extends Op
case class lineOp(i: Array[Double]) extends Op
case class columnOp(i: Array[Double]) extends Op
case class widthOp(i: Double) extends Op
case class genOp(i: Boolean) extends Op
case class lineMatrixOp(i: Array[Double]) extends Op
case class matrixOp(i: Matrix) extends Op
case class colorsOp(i: Array[String]) extends Op
case class formatOp(s: String, d: Double) extends Op
case class graduationOp(i: Int) extends Op
case class labelOp(i: Int) extends Op
case class orderOp(i : Boolean) extends Op