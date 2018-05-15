package example

import d3v4.d3
import example.ScalaJSExample._

object order {
  def is(o : Order) = {
    o.toString() match {
      case "descending" => orderOp(true)
      case "ascending"  => orderOp(false)
    }
  }
}
object graduation { def every(i : Int) = graduationOp(i)}
object label {def every(i: Int) = labelOp(i)}
object line {def apply(i : Double*) = {lineMatrixOp(i.toArray)}}
object regenerate {def is(i : Boolean) = genOp(i)}
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
object color {
  def is (i : String) = colorOp(i)
  def are (i: String*) = {val tmp : Array[String] = i.toArray; colorsOp(tmp)}
  def from (i : String) = {colorOp(d3.select("#"+i).property("value"))}
}
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
object origin {
  def from (i : Double*) = {columnOp(i.toArray)}
  def from (i:String) = {
    val tmp = d3.select("#"+i).property("value").split(",").map(_.toDouble)
    d3.select("#"+i).property("value","");
    columnOp(tmp)
  }
}
object destination {
  def from(i : String) = {
    val tmp = d3.select("#"+i).property("value").split(",").map(_.toDouble)
    d3.select("#"+i).property("value","");
    lineOp(tmp)
  }
  def to (i : Double*) = {lineOp(i.toArray)}
}
object  width { def is( i :Double) = widthOp(i)}