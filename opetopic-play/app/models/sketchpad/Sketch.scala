/**
  * Sketch.scala - A model for an Opetopic Sketch
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package models.sketchpad

import opetopic._
import opetopic.ui._
import markers._
import SimpleMarker._

class Sketch(
  val fc: FiniteComplex[OptMarker]
){

  type Dim = fc.N

  def complex: Complex[OptMarker, Dim] = 
    fc.value

}
