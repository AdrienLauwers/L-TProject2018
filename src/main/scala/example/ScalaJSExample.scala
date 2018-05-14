package example

import d3v4._
import example.ScalaJSExample.MyChordGraph.{generate}
import example.ScalaJSExample.generate

import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

  object ScalaJSExample {
    @JSExportTopLevel("myproject")
    protected def getInstance(): this.type = this

    def * :String ={
      return "all"
    }

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

    object select {
     def the(o : DO) = o match {
       case item => new itemClass()
       case _ => new itemClass()
      }
      def in (o: DO) = o match{
        case matrix => new readMatrix()
      }
    }

    object generate{
      def the(o: DO) = o match {
        case chord => MyChordGraph.generate()
        case _     => throw new Exception()
      }
    }

    class readMatrix {
      var myindices:Array[Int] = new Array[Int](4)
      var index =0
      def line(i: Int):readMatrix={
        myindices(index) = i
        index+=1
        return this
      }
      def line(i : String): readMatrix={
        myindices(0) = 0
        myindices(1) = MyChordGraph.matrix.length - 1
        index += 2
        return this
      }
      def to(i: Int):readMatrix={
        myindices(index) = i
        index+=1
        return this
      }

      def column(i: Int):readMatrix={
        myindices(index) = i
        index+=1
        return this
      }
      def column(i : String): readMatrix ={
        myindices(2) = 0
        myindices(3) = MyChordGraph.matrix(0).length - 1
        index += 2
        return this
      }
      def end(): Array[Array[Double]]={
        var test  = new ArrayBuffer[ArrayBuffer[Double]]()
        var i:Int;
        var j:Int;
        var index :Int =0;
        for(i <- myindices(0)-1 to myindices(1)-1){
          for( j<- myindices(2)-1 to myindices(3)-1){
            test(index) :+= MyChordGraph.matrix(j)(j)
          }
          index +=1
        }

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
    object item extends DO
    object matrix extends DO

    trait Op
    case class colorOp(i: String) extends Op
    case class lineOp(i: Array[Double]) extends Op
    case class columnOp(i: Array[Double]) extends Op
    case class widthOp(i: Double) extends Op

    object color {
      def is (i : String) = colorOp(i)
      def from (i : String) = {
        val tmp = d3.select("#"+i).property("value")
        colorOp(tmp)}
    }
    object data {
      def is (i : Double*) = {val tmp : Array[Double] = i.toArray; lineOp(tmp)}
      def from (i : Double*) = {val tmp : Seq[Double] = i; columnOp(tmp.toArray)}
      def to (i : Double*) = {val tmp : Seq[Double] = i; lineOp(tmp.toArray)}
    }
    object origin {
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
    }
    object  width { def is( i :Double) = widthOp(i)}

    class itemClass
    {
      def named(name : String) : Action = { new Action(name)}
    }

    class ChordGraph {
      def where(op: Op) = MyChordGraph(op)
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
      def resetData() = {
       // matrix = List(List(50))
       // colors = List("#FFDD89")
        generate()
      }
      var matrix = new ArrayBuffer[ArrayBuffer[Double]]()
      var colors = new ArrayBuffer[String]()

      def addElem(op: Op*) = {
        op.foreach( _ match {
          case lineOp(i) => {
            import js.JSConverters._
            println("Avant le line op")
            matrix += i.to[ArrayBuffer]
            println("Après le line op")
            println(matrix)
          }
          case columnOp(i) => {
            for((x,j) <- matrix.view.zipWithIndex){matrix(j)+= i(j)}
          }
          case colorOp(i) => { colors = colors :+ i; println(colors)}
          case _ => println(op);
        })
        generate();
      }

      def apply(op: Op*)  = {
        op.foreach( _ match {
          case widthOp(i) => {d3.select("svg").attr("width", i); d3.select("svg").attr("height", i)}
        })
      }

      def generate()  = {
        import js.JSConverters._
        var colorJS : js.Array[String] = colors.toJSArray
        var matrixJS : js.Array[js.Array[Double]] = new js.Array[js.Array[Double]]()
        matrix.foreach(x => matrixJS.push(x.toJSArray))
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

      def multiply(x: Double) = {

      }

    }


    @JSExport
    def main(args: Array[String]): Unit = {
      create a chord where (width is 960.0)
      add elemTo chord having (data is (55), color is "#FFDD89");
      generate the chord
      println("mabite dans le cul à corrado")
      var mabite = select in matrix line 1 to 5 column * end;
      select the item named "sizeButton" make resizeFunction
      select the item named "resetButton" make resetDataFunction
      select the item named "addDataButton" make {() =>
        add elemTo chord having (origin from "originInput", destination from "destinationInput", color from "colorInput")}
     // chord + 12
    }

    def resizeFunction() = {
      import d3v4.d3
      println(d3.select("#from").property("value"))
    }

    def resetDataFunction() = {
      println("test")
      MyChordGraph.resetData();
    }

    def addDataFunction() = {
      d3.select("svg").attr("width", 100)
    }


}

