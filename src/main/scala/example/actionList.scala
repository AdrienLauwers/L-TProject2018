package example

import example.ScalaJSExample._
/*This package contains all verb of the DSL
* Each verb have an effect on a object
* */

/*
Object reset allows the user to reset the chord
 */
object reset {def the(o : DO) = o match{ case chord => MyChordGraph.resetData()}}

/*
Object create allows the user to create the chord or a matrix
 */
object create {
  def the(o: DO) = o match {case chord => new ChordGraph()}
  def a(o: DO) = o match {case matrix => new Matrix()}
}
/*
Object select allows the user to select information inside a Matrix or a Chord
 */
object select {
  def the(o : DO) = o match {case item => new itemClass()}
  def in (o: DO) = o match{case chord => new readMatrix()}
}

/*
Object generate allows the user to generate the chord.
 */
object generate{ def the(o: DO) = o match {case chord => MyChordGraph.generate()}}
/*
Object add allows the user to add data the chord
 */
object add {def elemTo(o: DO) = o match {case chord => new ChordGraph()}}
