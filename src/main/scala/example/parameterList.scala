package example

import d3v4.d3
import example.ScalaJSExample._

/*
* This is the implementation of the differents operators.
*/


/*
* Object order is use to recognize when the user will insert a specific order in a chord (ie ascendig or descending).
 */
object order {
  def is(o : Order) = {
    o.toString() match {
      case "descending" => orderOp(true)
      case "ascending"  => orderOp(false)
    }
  }
}

/*
* Object graduation is use to recognize when the user will insert a specific graduation in a chord.
 */
object graduation { def every(i : Int) = graduationOp(i)}
/*
* Object label is use to recognize when the user will insert a specific label in a chord.
 */
object label {def every(i: Int) = labelOp(i)}
/*
* Object line is use to recognize when the user will insert a line in a chord.
 */
object line {def apply(i : Double*) = {lineMatrixOp(i.toArray)}}

/*
* Object regenerate is use to recognize when the user will generate the chord object.
 */
object regenerate {def is(i : Boolean) = genOp(i)}

/*
* Object format is use to recognize when the user will insert a specific format in a chord.
 */
object format{
  def is(f : FormatType) = {
    f.toString() match {
      case "dot" => formatOp(".^20",42) // dot-filled and centered, ".........42........."
      case "money" => formatOp("($.2f",-3.5) // localized fixed-point currency, "(£3.50)"
      case "space" => formatOp("+20",42) // space-filled and signed, "                 +42"
      case "thousand" => formatOp(",.0", 1e3)
    }
  }
}

/*
* Object color is use to recognize when the user will insert a specific color or a list of colors in a chord.
 */
object color {
  def is (i : String) = colorOp(i)
  def are (i: String*) = {val tmp : Array[String] = i.toArray; colorsOp(tmp)}
  def from (i : String) = {colorOp(d3.select("#"+i).property("value"))}
}

/*
* Object data is use to recognize when the user will insert a specific matrix inside a chord, or add data inside the matrix of a chord.
 */
object data {
  def is (i : Matrix) = matrixOp(i)
  def is (i : Double*) = {val tmp : Array[Double] = i.toArray; lineOp(tmp)}
  def from (i : Double*) = {val tmp : Seq[Double] = i; lineOp(tmp.toArray)}
  def to (i : Double*) = {val tmp : Seq[Double] = i; columnOp(tmp.toArray)}
  def from (i:String) = {
    val tmp = d3.select("#"+i).property("value").split(",").map(_.toDouble)
    d3.select("#"+i).property("value","");
    lineOp(tmp)
  }
}
/*
* Object origin is use to recognize when the user will insert a specific column inside the chord from the DSL or from the web page.
 */
object origin {
  def from (i : Double*) = {columnOp(i.toArray)}
  def from (i:String) = {
    val tmp = d3.select("#"+i).property("value").split(",").map(_.toDouble)
    d3.select("#"+i).property("value","");
    columnOp(tmp)
  }
}
/*
* Object destination is use to recognize when the user will insert a specific line inside the chord from the DSL or from the web page.
 */
object destination {
  def from(i : String) = {
    val tmp = d3.select("#"+i).property("value").split(",").map(_.toDouble)
    d3.select("#"+i).property("value","");
    lineOp(tmp)
  }
  def to (i : Double*) = {lineOp(i.toArray)}
}
/*
* Object width is use to recognize when the user will insert a specific width for the chord.
 */
object  width { def is( i :Double) = widthOp(i)}