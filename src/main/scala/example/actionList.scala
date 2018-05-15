package example

import example.ScalaJSExample._

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
