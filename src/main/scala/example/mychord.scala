package example

import d3v4._
import example.ScalaJSExample._

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js

/** This object is used to get all the information
  * and parameter provided by the user and generate the chord by using them
  */
object MyChordGraph extends DO {
  /** Data to use in the chord **/
  var matrix = new Matrix();
  /** Colors to user for each element in the chord **/
  var colors = new ArrayBuffer[String]()
  /** Format to use, the format by default is money **/
  var format1 = "($.2f"
  var format2 = -3.5
  /** Specify that we want a label on the graduation on each X numbers, by default it's every 10 **/
  var labelEvery = 10
  /** Specify that we want a  graduation on each X numbers, by default it's every 10 **/
  var graduationEvery = 10
  /** Specify the order of display **/
  var descending = true

  /** Get the data **/
  def data = matrix.data

  /** Get the number of lines **/
  def lineLength = matrix.lineLength
  /** Get the number of columns **/
  def columnLength = matrix.columnLength

  /** Used to add a new elements to the chord
    * we have to provice a new line, a new column, a boolean to know if we want to generate the chord now
    * and the color to use
    */
  def addElem(op: Op*) = {
    op.foreach( _ match {
      case lineOp(i) => {
        matrix.addLine(i)
      }
      case columnOp(i) => {
        matrix.addColumn(i)
      }
      case genOp(i) => {
        if(i){generate()}
      }
      case colorOp(i) => { colors = colors :+ i; println(colors)}
      case _ => println(op)
    })

  }

  /** Used to get all the parameters of the chord a the construction **/
  def construct(op: Op*)  = {
    op.foreach( _ match {
      case widthOp(i) => {d3.select("svg").attr("width", i); d3.select("svg").attr("height", i)}
      case matrixOp(i) => {matrix = i}
      case colorsOp(i) => {colors = i.to[ArrayBuffer]}
      case formatOp(i,d) => {
        format1 = i ;
        format2 = d;}
      case labelOp(i) => labelEvery = i;
      case graduationOp(i) => graduationEvery=i;
      case orderOp(i) => descending = i
    })
  }

  /** Generate the graph with the provided information **/
  def generate()  = {
    import js.JSConverters._
    var colorJS : js.Array[String] = colors.toJSArray
    var matrixJS : js.Array[js.Array[Double]] = new js.Array[js.Array[Double]]()
    matrix.data.foreach(x => matrixJS.push(x.toJSArray))

    d3.select("#matrixArea").property("value",matrix.displayMatrix());

    import d3v4.d3
    val svg = d3.select("svg")
    val width = svg.attr("width").toDouble
    val height = svg.attr("height").toDouble
    val outerRadius = Math.min(width, height) * 0.5 - 40
    println(outerRadius)
    val innerRadius = outerRadius - 30
    println(innerRadius)
    val formatValue = d3.formatPrefix(format1,format2)

    val chord = if (descending) d3.chord().padAngle(0.05).sortSubgroups(d3.descending) else d3.chord().padAngle(0.05).sortSubgroups(d3.ascending)

    val arc = d3.arc().innerRadius(innerRadius).outerRadius(outerRadius)
    val ribbon = d3.ribbon().radius(innerRadius)

    val color = d3.scaleOrdinal[Int, String]().domain(d3.range(5)).range(colorJS)
    val g: Selection[ChordArray] = svg.append("g").attr("transform", "translate(" + width / 2 + "," + height / 2 + ")").datum(chord(matrixJS))
    val group = g.append("g").attr("class", "groups")
      .selectAll("g")
      .data((c: ChordArray) => c.groups)
      .enter().append("g")

    var groupTick = group.selectAll(".group-tick").data((d: ChordGroup) => groupTicks(d, graduationEvery))
      .enter().append("g").attr("class", "group-tick")
      .attr("transform", (d: js.Dictionary[Double]) =>  "rotate(" + (d("angle") * 180 / Math.PI - 90) + ") translate(" + outerRadius + ",0)")

    groupTick.append("line").attr("x2", 6)

    groupTick.filter((d: js.Dictionary[Double]) => d("value") % labelEvery == 0).append("text")
      .attr("x", 8)
      .attr("dy", ".35em")
      .attr("transform", (d: js.Dictionary[Double]) => if(d("angle") > Math.PI) "rotate(180) translate(-16)" else null)
      .style("text-anchor", (d: js.Dictionary[Double]) => if(d("angle") > Math.PI) "end" else null)
      .text((d: js.Dictionary[Double]) => formatValue(d("value")))

    g.append("g").attr("class", "ribbons").selectAll("path").data((c: ChordArray) => c)
      .enter().append("path")
      .attr("d", (d: Chord) => ribbon(d))
      .style("fill", (d: Chord) => color(d.target.index))
      .style("stroke", (d: Chord) => d3.rgb(color(d.target.index)).darker())
  }

  /** Reset all the data provided but not the parameters**/
  def resetData() = {
    matrix.resetData()
    colors = new ArrayBuffer[String]()
    generate()
  }
}