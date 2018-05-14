package example

import d3v4._


import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object ScalaJSExample {
  @JSExport
  def main(args: Array[String]): Unit = {
    var matrix1 = create a matrix having (
      line(50,20),
      line(30,10)
    )
    var matrix2 = create a matrix having(
      line(2,3),
      line(8,1)
    )
    var matrix3 = create a matrix having(
      line(1,1),
      line(2,1)
    )
    create the chord where (width is 1000,
      data is matrix1+matrix2,
      color are ("#000000","#00E6FF"),
      format is money,
      label every 100,
      graduation every 100)

    //create the chord where (width is 1000,
    //                        data is matrix1*matrix3*2,
    //                        color are ("#000000","#00E6FF","#3C00FF"))

    add elemTo chord having (origin from (20,20,20),
                            destination to (20,20,20),
                            color is "#09FF00");
                            generate the chord

    select in chord column 1 to 2 line * display;

    select the item named "resetButton" make (() => reset the chord)

    select the item named "addDataButton" make {() =>
      add elemTo chord having (origin from "originInput",
        destination from "destinationInput",
        color from "colorInput",
        regenerate is true)}
    
    select the item named "addFirstDataButton" make{() =>
      add elemTo chord having (data from "firstInput",
        color from "colorInput",
        regenerate is true);
    }
  }

    @JSExportTopLevel("myproject")
    protected def getInstance(): this.type = this
    def * :String ={
      return "all"
    }

    def groupTicks(d: ChordGroup, step: Double): js.Array[js.Dictionary[Double]] = {
      val k: Double = (d.endAngle - d.startAngle) / d.value
      d3.range(0, d.value, step).map((v: Double) => js.Dictionary("value" -> v, "angle" -> (v * k + d.startAngle)))
    }

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

  object reset {def the(o : DO) = o match{ case chord => MyChordGraph.resetData()}}
  object create {
    def the(o: DO) = o match {case chord => new ChordGraph()}
    def a(o: DO) = o match {case matrix => new Matrix()}
  }
  object select {
    def the(o : DO) = o match {case item => new itemClass()}
    def in (o: DO) = o match{case chord => new readMatrix()}
  }
  object generate{ def the(o: DO) = o match {case chord => MyChordGraph.generate()}}
  object add {def elemTo(o: DO) = o match {case chord => new ChordGraph()}}

  trait DO
  object chord extends DO {def select() = new readMatrix()}
  object item extends DO
  object matrix extends DO {
    def empty(line : Int, col : Int) : Matrix = {
      var tmp = ArrayBuffer[ArrayBuffer[Double]]();
      var tmp2 = Array.ofDim[Double](line, col);
      for( j <- 0 to tmp2.size-1){
        println(j)
        tmp += tmp2(j).to[ArrayBuffer];
      }
      return new Matrix(tmp)
    }
  }

  class readMatrix {
      var myindices:Array[Int] = new Array[Int](4)
      var isline:Boolean=true;
      def line(i: Int):readMatrix={
        myindices(0) = i
        return this
      }
      def line(i : String): readMatrix={
        myindices(0) = 1
        myindices(1) = MyChordGraph.lineLength
        return this
      }
      def to(i: Int):readMatrix={
        if(isline){
          myindices(1) = i
        }
        else{
          myindices(3) = i
        }
        return this
      }
      def column(i: Int):readMatrix={
        myindices(2) = i
        isline = false;
        return this
      }
      def column(i : String): readMatrix ={
        myindices(2) = 1
        myindices(3) = MyChordGraph.columnLength
        isline=false
        return this
      }
      def end(): ArrayBuffer[ArrayBuffer[Double]]={
        var test  = new ArrayBuffer[ArrayBuffer[Double]]()
        var i:Int =0;
        var j:Int=0;
        var index :Int =0;
        for(i <- myindices(0)-1 to myindices(1)-1){
          test += new ArrayBuffer[Double](0)
          for( j<- myindices(2)-1 to myindices(3)-1){
            test(index) += MyChordGraph.data(i)(j)
          }
          index +=1
        }
        return test
      }
      def display(): Unit =
      {
        println(end())
      }
    }

    trait Format
    case object money extends Format
    case object space extends Format
    case object dot extends Format
    case object thousand extends Format

    object graduation { def every(i : Int) = graduationOp(i)}
    object label {def every(i: Int) = labelOp(i)}
    object line {def apply(i : Double*) = {lineMatrixOp(i.toArray)}}
    object regenerate {def is(i : Boolean) = genOp(i)}
    object format{
     def is(f : Format) = {
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

    class itemClass
    {
      def named(name : String) : Action = { new Action(name)}
    }

    class ChordGraph {
      def where(op: Op*) = MyChordGraph(op:_*)
      def having(op: Op*) = MyChordGraph.addElem(op:_*)
    }

    class Action(name : String) {
      def make(fnct: () => Unit) = {
        d3.scaleLinear()
        println(name)
        d3.select("#"+name).on("click", () => fnct())
      }
    }

    object MyChordGraph extends DO {
      var matrix = new Matrix();
      var colors = new ArrayBuffer[String]()
      var format1 = ".0%"
      var format2 = 0.123
      var labelEvery = 10
      var graduationEvery = 10

      def data = matrix.data

      def lineLength = matrix.lineLength
      def columnLength = matrix.columnLength

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

      def apply(op: Op*)  = {
        op.foreach( _ match {
          case widthOp(i) => {d3.select("svg").attr("width", i); d3.select("svg").attr("height", i)}
          case matrixOp(i) => {matrix = i}
          case colorsOp(i) => {colors = i.to[ArrayBuffer]}
          case formatOp(i,d) => {
            format1 = i ;
            format2 = d;}
          case labelOp(i) => labelEvery = i;
          case graduationOp(i) => graduationEvery=i;
        })
      }

      def generate()  = {
        import js.JSConverters._
        var colorJS : js.Array[String] = colors.toJSArray
        var matrixJS : js.Array[js.Array[Double]] = new js.Array[js.Array[Double]]()
        matrix.data.foreach(x => matrixJS.push(x.toJSArray))
        import d3v4.d3
        val svg = d3.select("svg")
        val width = svg.attr("width").toDouble
        val height = svg.attr("height").toDouble
        val outerRadius = Math.min(width, height) * 0.5 - 40
        val innerRadius = outerRadius - 30
        val formatValue = d3.formatPrefix(format1,format2)
        val chord = d3.chord().padAngle(0.05).sortSubgroups(d3.descending)
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

      def resetData() = {
        matrix.resetData()
        colors = new ArrayBuffer[String]()
        generate()
      }
    }
}
