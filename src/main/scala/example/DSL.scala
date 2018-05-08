package example

import d3v4.{Chord, ChordArray, ChordGroup, Selection}
import example.ScalaJSExample.groupTicks

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}



object DSL {
  @JSExportTopLevel("myproject")
  object create {
    def a(o: DO) = o match {
      case chord => new ChordGraph()
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
  object chord extends DO {
    def generate() =  MyChordGraph.generate()
  }

  trait Op
  case class colorOp(i: String) extends Op
  case class dataOp(i: Seq[Int]) extends Op
  object color { def is (i : String) = colorOp(i) }
  object data {def are (i : Int*) = {val tmp : Seq[Int] = i; dataOp(tmp)}}

  class ChordGraph {
    def where (op: Op*) = MyChordGraph(op:_*)
    def having (op: Op*) = MyChordGraph.addElem(op:_*)
  }

  object MyChordGraph extends DO {

    var matrix  : List[Seq[Int]] = List()
    var colors :  List[String] = List()
    var test : String = ""


    def addElem(op: Op*) = {
      op.foreach( _ match {
        case dataOp(i) => {
          matrix = matrix :+ i;
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
      //var matrixJS : js.Array[String] = js.Array("test")
      import d3v4.d3
      val svg = d3.select("svg")
      val width = svg.attr("width").toDouble
      val height = svg.attr("height").toDouble
      val outerRadius = Math.min(width, height) * 0.5 - 40
      val innerRadius = outerRadius - 30

      /*val formatValue = d3.formatPrefix(",.0", 1e3)

      val chord = d3.chord().padAngle(0.05).sortSubgroups(d3.descending)

      val arc = d3.arc().innerRadius(innerRadius).outerRadius(outerRadius)

      val ribbon = d3.ribbon().radius(innerRadius)

      val color = d3.scaleOrdinal[Int, String]().domain(d3.range(5)).range(colorJS)

      val g: Selection[ChordArray] = svg.append("g").attr("transform", "translate(" + width / 2 + "," + height / 2 + ")").datum(chord(matrix))

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
        .style("stroke", (d: Chord) => d3.rgb(color(d.target.index)).darker()) */

    }
  }


  @JSExport
  def main(args: Array[String]): Unit = {
  //  val str = cons(1,cons(2,Stream.empty))

    create a chord ; // a remplacer par width
    add elemTo chord having (data are (12,15,15), color is "#FFDD89");
    add elemTo chord having (data are (10,5,55), color is "#957244");
    add elemTo chord having (data are (12,45,65), color is "#F26223");
    chord generate


  //  val b =  ( 5.5  | cons(1, Stream.Empty))
  }
/*
  implicit class EncogArray1[Double](val elem1: Double) {
    def |(v:  Stream[Double]) = cons(elem1, v)
  }

  import ComplexImplicits._
  object ComplexImplicits {
    implicit def doubleToStream(d: Double) =cons(d, Stream.Empty)
  }
*/

}
