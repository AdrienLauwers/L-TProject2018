package example

import d3v4._


import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import example._

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
                      label every 10,
                      graduation every 10,
                      order is descending,
                      )

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


  trait Order
    case object descending extends Order
    case object ascending extends Order

    class itemClass
    {
      def named(name : String) : Action = { new Action(name)}
    }

    class ChordGraph {
      def where(op: Op*) = MyChordGraph.construct(op:_*)
      def having(op: Op*) = MyChordGraph.addElem(op:_*)
    }

    class Action(name : String) {
      def make(fnct: () => Unit) = {
        d3.scaleLinear()
        println(name)
        d3.select("#"+name).on("click", () => fnct())
      }
    }


}
