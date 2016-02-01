/**
  * CellMarker.scala - A Class for Labelling Cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import opetopic._
import opetopic.ui._
import opetopic.js._
import JsDomFramework._

case class CellMarker[N <: Nat](
  val label: String,
  val fillColor: String,
  val strokeColor: String
)

object CellMarker {

  implicit object CellMarkerFamily extends AffixableFamily[CellMarker] {
    def apply[N <: Nat](n: N) : Affixable[CellMarker[N]] = 
      new Affixable[CellMarker[N]] {

        type ElementType = TextType

        def decoration(mk: CellMarker[N]) : Decoration[TextType] =
          Decoration(text(mk.label))
      }
  }

}
