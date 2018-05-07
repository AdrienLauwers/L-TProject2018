package example

import scala.collection.immutable.Stream.cons
import scala.scalajs.js

object DSL {
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
  object chord extends DO

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
    //var matrix  : Array[Seq[Int]] = js.Array()
    var color :  js.Array[String]= new js.Array()
    var test : String = ""

    def addElem(op: Op*) = {
      op.foreach( _ match {
        case dataOp(i) => {
         // matrix = matrix ++ i
        }
        case colorOp(i) => {color.push(i); println(color)}
        case _ => println(op);
      })
    }

    def apply(op: Op*)  = {
      op.foreach( _ match {
        //case matrixOp(i) => matrix.push(i)
        case colorOp(i) => test = i
      })
    }
  }



  def main(args: Array[String]): Unit = {
  //  val str = cons(1,cons(2,Stream.empty))
    create a chord where (color is "red"); // a remplacer par width
    add elemTo chord having (data are (12,15,15), color is "red");


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
