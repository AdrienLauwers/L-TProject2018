package example

import d3v4._

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object ScalaJSExample {
  @JSExportTopLevel("myproject")
  protected def getInstance(): this.type = this

  def groupTicks(d: ChordGroup, step: Double): js.Array[js.Dictionary[Double]] = {
    val k: Double = (d.endAngle - d.startAngle) / d.value
    d3.range(0, d.value, step).map((v: Double) => js.Dictionary("value" -> v, "angle" -> (v * k + d.startAngle)))
  }

  object create {
    def a(o: DO) = o match {
      case chord => new ChordGraph()
      //case geoMap => new geoMap()
      case _     => throw new Exception()
    }
  }

  object generate{
    def the(o: DO) = o match {
      case chord => MyChordGraph.generate()
      case geoMap => new geoMap()
      case _     => throw new Exception()
    }
  }

  object add {
    def elemTo(o: DO) = o match {
      case chord => new ChordGraph()
      case _     => throw new Exception()
    }
  }

  trait DO
  object chord extends DO
  object geoMap extends DO

  trait Op
  case class colorOp(i: String) extends Op
  case class dataOp(i: Seq[Double]) extends Op
  object color { def is (i : String) = colorOp(i) }
  object data {def are (i : Double*) = {val tmp : Seq[Double] = i; dataOp(tmp)}}

  class ChordGraph {
    def where(op: Op*) = MyChordGraph(op:_*)
    def having(op: Op*) = MyChordGraph.addElem(op:_*)
  }

  class geoMap() {
    def tamere(op: Op*) = MyChordGraph(op:_*)
  }

  object MyChordGraph extends DO {
    var matrix  : List[js.Array[Double]] = List()
    var colors :  List[String] = List()
    var test : String = ""
    def addElem(op: Op*) = {
      op.foreach( _ match {
        case dataOp(i) => {
          import js.JSConverters._
          matrix = matrix :+ i.toJSArray;
        }
        case colorOp(i) => { colors = colors :+ i; println(colors)}
        case _ => println(op);
      })
    }

    def apply(op: Op*)  = {
      op.foreach( _ match {
        //case matrixOp(i) => matrix.push(i)
        case colorOp(i) => test = i
      })
    }

    def generate()  = {
      import js.JSConverters._
      var colorJS : js.Array[String] = colors.toJSArray
      var matrixJS : js.Array[js.Array[Double]] = matrix.toJSArray
      import d3v4.d3
      val svg = d3.select("svg")
      val width = svg.attr("width").toDouble
      val height = svg.attr("height").toDouble
      val outerRadius = Math.min(width, height) * 0.5 - 40
      val innerRadius = outerRadius - 30

      val formatValue = d3.formatPrefix(",.0", 1e3)

      val chord = d3.chord().padAngle(0.05).sortSubgroups(d3.descending)

      val arc = d3.arc().innerRadius(innerRadius).outerRadius(outerRadius)

      val ribbon = d3.ribbon().radius(innerRadius)

      val color = d3.scaleOrdinal[Int, String]().domain(d3.range(5)).range(colorJS)

      val g: Selection[ChordArray] = svg.append("g").attr("transform", "translate(" + width / 2 + "," + height / 2 + ")").datum(chord(matrixJS))

      val group = g.append("g").attr("class", "groups")
        .selectAll("g")
        .data((c: ChordArray) => c.groups)
        .enter().append("g")


      group.append("path").style("fill", (d: ChordGroup) => color(d.index))
        .style("stroke", (d: ChordGroup) => d3.rgb(color(d.index)).darker())
        .attr("d", (x: ChordGroup) => arc(x))

      var groupTick = group.selectAll(".group-tick").data((d: ChordGroup) => groupTicks(d, 1e3))
        .enter().append("g").attr("class", "group-tick")
        .attr("transform", (d: js.Dictionary[Double]) =>  "rotate(" + (d("angle") * 180 / Math.PI - 90) + ") translate(" + outerRadius + ",0)")

      groupTick.append("line").attr("x2", 6)

      groupTick.filter((d: js.Dictionary[Double]) => d("value") % 5e3 == 0).append("text")
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
  }


  @JSExport
  def main(args: Array[String]): Unit = {
    create a chord ; // a remplacer par width
    add elemTo chord having (data are (12,15,15), color is "#FFDD89");
    add elemTo chord having (data are (10,5,55), color is "#957244");
    add elemTo chord having (data are (12,45,65), color is "#F26223");
    generate the chord;
  }
}
