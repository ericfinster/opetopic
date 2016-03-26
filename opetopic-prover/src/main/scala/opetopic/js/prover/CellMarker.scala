/**
  * CellMarker.scala - Markers for attaching to editor cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import opetopic._
import opetopic.js._
import opetopic.tt._
import opetopic.ui._
import syntax.complex._

sealed trait CellMarker[N <: Nat] {

  def dim: N

  def displayName: String

  def visualize(frmwk: UIFramework) : frmwk.Visualization[N]

}

case class ObjectMarker(val displayName: String) extends CellMarker[_0] {

  def dim = Z

  def visualize(frmwk: UIFramework) : frmwk.Visualization[_0] = {

    import frmwk._

    ObjectVisualization(
      DefaultColorSpec,
      text(displayName)
    )

  }

}


object CellMarker {

  object ActiveInstance {

    import JsDomFramework._

    implicit def cellMarkerVis[N <: Nat] : Visualizable[CellMarker[N], N] = 
      new Visualizable[CellMarker[N], N] {
        def visualize(mk: CellMarker[N]) : Visualization[N] = 
          mk.visualize(JsDomFramework)
      }

    implicit val cellMarkerVisFam : VisualizableFamily[CellMarker] = 
      new VisualizableFamily[CellMarker] {
        def visualize[N <: Nat](n: N)(mk: CellMarker[N]) : Visualization[N] = 
          cellMarkerVis[N].visualize(mk)
      }

  }

}
