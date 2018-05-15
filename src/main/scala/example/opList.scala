package example
/*
* List of the different operator possible in the DSL
* Each parameter has a effect on a noun (ie Matrix or Chord).
 */
/*
* Trait Op is the super trait of all operator to recognize it
*/
trait Op
/*
* Case class colorOp is use to recognize when the user will insert a color in a chord
*/
case class colorOp(i: String) extends Op
/*
* Case class linerOp is use to recognize when the user will insert a line in a chord
*/
case class lineOp(i: Array[Double]) extends Op
/*
* Case class columnOp is use to recognize when the user will insert a column in a chord
*/
case class columnOp(i: Array[Double]) extends Op
/*
* Case class widthOp is use to recognize when the user will insert a width in a chord
*/
case class widthOp(i: Double) extends Op
/*
* Case class genOp is use to recognize when the user will generate the chord
*/
case class genOp(i: Boolean) extends Op
/*
* Case class lineMatrixOp is use to recognize when the user will insert a line in a matrix
*/
case class lineMatrixOp(i: Array[Double]) extends Op
/*
* Case class matrixOp is use to recognize when the user will insert a matrix in a chord
*/
case class matrixOp(i: Matrix) extends Op
/*
* Case class colorsOp is use to recognize when the user will insert a list of colors in a chord
 */
case class colorsOp(i: Array[String]) extends Op
/*
* Case class formatOp is use to recognize when the user will insert a format in a chord
 */
case class formatOp(s: String, d: Double) extends Op
/*
* Case class graduationOp is use to recognize when the user will insert a specific graduation in a chord
 */
case class graduationOp(i: Int) extends Op
/*
* Case class labelOp is use to recognize when the user will insert a specific label in a chord
 */
case class labelOp(i: Int) extends Op
/*
* Case class orderOp is use to recognize when the user will insert a specific order in a chord (ie ascendig or descending)
 */
case class orderOp(i : Boolean) extends Op