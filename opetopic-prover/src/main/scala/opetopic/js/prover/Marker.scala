/**
  * Marker.scala - Markers for attaching to editor cells
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

sealed trait Marker[N <: Nat] {

  def dim: N

  def displayName: String
  def expr: Expr
  def tyExpr: Expr

  def visualize(frmwk: UIFramework) : frmwk.Visualization[N]

}

case class ObjectMarker(
  val displayName: String,
  val expr: Expr,
  val tyExpr: Expr
) extends Marker[_0] {

  def dim = Z

  def visualize(frmwk: UIFramework) : frmwk.Visualization[_0] = {

    import frmwk._

    ObjectVisualization(
      DefaultColorSpec,
      text(displayName)
    )

  }

}

case class CellMarker[P <: Nat](p: P)(
  val displayName: String,
  val expr: Expr,
  val tyExpr: Expr
) extends Marker[S[P]] {

  def dim = S(p)

  def visualize(frmwk: UIFramework) : frmwk.Visualization[S[P]] = {

    import frmwk._

    CellVisualization(
      DefaultColorSpec,
      text(displayName)
    )

  }

}

object Marker {

  object ActiveInstance {

    import JsDomFramework._

    implicit def cellMarkerVis[N <: Nat] : Visualizable[Marker[N], N] = 
      new Visualizable[Marker[N], N] {
        def visualize(mk: Marker[N]) : Visualization[N] = 
          mk.visualize(JsDomFramework)
      }

    implicit val cellMarkerVisFam : VisualizableFamily[Marker] = 
      new VisualizableFamily[Marker] {
        def visualize[N <: Nat](n: N)(mk: Marker[N]) : Visualization[N] = 
          cellMarkerVis[N].visualize(mk)
      }

  }

}
