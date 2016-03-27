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

  def visualize(frmwk: UIFramework) : frmwk.Visualization[N]

  def cs : ColorSpec = 
    expr match {
      case EVar(_) => VarColorSpec
      case _ => DefaultColorSpec
    }

}

case class ObjectMarker(
  val displayName: String,
  val expr: Expr
) extends Marker[_0] {

  val dim: _0 = Z

  def visualize(frmwk: UIFramework) : frmwk.Visualization[_0] =
    frmwk.ObjectVisualization(cs, frmwk.text(displayName))

}

case class CellMarker[P <: Nat](p: P)(
  val displayName: String,
  val expr: Expr
) extends Marker[S[P]] {

  val dim: S[P] = S(p)

  def visualize(frmwk: UIFramework) : frmwk.Visualization[S[P]] = 
    frmwk.CellVisualization(cs, frmwk.text(displayName))

}

object VarColorSpec extends ColorSpec(
  fill = "#FFE21F",
  fillHovered = "#FDE21F",
  fillSelected = "#FBBD08",
  stroke = "#000000",
  strokeHovered = "#000000",
  strokeSelected = "#000000"
)

object Marker {

  @natElim
  def apply[N <: Nat](n: N)(id: String, expr: Expr) : Marker[N] = {
    case (Z, id, expr) => ObjectMarker(id, expr)
    case (S(p), id, expr) => CellMarker(p)(id, expr)
  }

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
