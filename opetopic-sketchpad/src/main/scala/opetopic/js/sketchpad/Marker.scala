/**
  * Marker.scala - Cell visualization marker
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import opetopic._
import opetopic.ui._
import opetopic.js._
import syntax.tree._

sealed trait Marker[N <: Nat] {
  def label: String
  def colorSpec: ColorSpec
}

case class ObjectMarker(
  val label: String,
  val colorSpec: ColorSpec = DefaultColorSpec
) extends Marker[_0] 

case class CellMarker[P <: Nat](
  val label: String,
  val colorSpec: ColorSpec = DefaultColorSpec,
  val rootEdgeDecoration: TriangleDec = Nonexistant,
  val leafEdgeDecorations: Option[Tree[TriangleDec, P]] = None
) extends Marker[S[P]] 

sealed trait TriangleDec {

  def next : TriangleDec = 
    this match {
      case Nonexistant => BlackTriangle
      case BlackTriangle => RedTriangle
      case RedTriangle => Nonexistant
    }

}

case object Nonexistant extends TriangleDec
case object BlackTriangle extends TriangleDec
case object RedTriangle extends TriangleDec

object Marker {

  type OptMarker[N <: Nat] = Option[Marker[N]]

  @natElim
  def apply[N <: Nat](n: N)(lbl: String, spec: ColorSpec) : Marker[N] = {
    case (Z, lbl, spec) => ObjectMarker(lbl, spec)
    case (S(p), lbl, spec) => CellMarker(lbl, spec)
  }

  object ActiveInstance {

    import JsDomFramework._

    def lblEl(lbl: String) = 
      if (lbl == "") spacer(Bounds(0,0,600,600)) else text(lbl)

    def triUp(c: String) = polygon(c, 100, c, List((150, 0), (300, 300), (0, 300)))
    def triDown(c: String) = polygon(c, 100, c, List((0, 0), (150, 300), (300, 0)))
    def bnds = Bounds(0, 0, 300, 300)

    def renderUp(d: TriangleDec) : Option[BoundedElement] =
      d match {
        case Nonexistant => None
        case BlackTriangle => Some(BoundedElement(triUp("black"), bnds))
        case RedTriangle => Some(BoundedElement(triUp("red"), bnds))
      }

    def renderDown(d: TriangleDec) : Option[BoundedElement] =
      d match {
        case Nonexistant => None
        case BlackTriangle => Some(BoundedElement(triDown("black"), bnds))
        case RedTriangle => Some(BoundedElement(triDown("red"), bnds))
      }

    implicit val markerFamily : VisualizableFamily[Marker] = 
      new VisualizableFamily[Marker] {
        @natElim
        def visualize[N <: Nat](n: N)(mk: Marker[N]) : Visualization[N] = {
          case (Z, ObjectMarker(lbl, spec)) => ObjectVisualization(spec, lblEl(lbl))
          case (S(p: P), CellMarker(lbl, spec, rd, eds)) => {

            val markRoot = renderDown(rd)
            val markLeaves = eds map ((tr : Tree[TriangleDec, P]) => {
              tr map renderUp
            })

            CellVisualization(spec, lblEl(lbl), markRoot, markLeaves)

          }
        }
      }

  }

  object StaticInstance {

    import opetopic.ui.ScalatagsTextFramework._

    implicit val skinFamily : VisualizableFamily[Marker] = ???

  }

}

// // object CellMarker {

// //   type OptCellMarker[N <: Nat] = Option[CellMarker[N]]

// //   object ActiveInstance {

// //     import JsDomFramework._

// //     implicit object CellMarkerFamily extends VisualizableFamily[CellMarker] {

// //       def spcr = spacer(Bounds(0,0,600,600))
// //       def emkr = rect(0, 0, 300, 300, 100, "red", 100, "red")

// //       @natElim
// //       def visualize[N <: Nat](n: N)(mk: CellMarker[N]) : Visualization[N] = {
// //         case (Z, mk) => Visualization(Z)(mk.colorSpec, if (mk.label == "") spcr else text(mk.label))
// //         case (S(p), mk) => {
// //           CellVisualization(
// //             mk.colorSpec,
// //             if (mk.label == "") spcr else text(mk.label),
// //             Some(BoundedElement(emkr, Bounds(0, 0, 300, 300))),
// //             None
// //           )
// //         }
// //       }
// //     }

// //   }

// //   object StaticInstance {

// //     import opetopic.ui.ScalatagsTextFramework._

// //     implicit object CellMarkerFamily extends VisualizableFamily[CellMarker] {
// //       def visualize[N <: Nat](n: N)(mk: CellMarker[N]) : Visualization[N] = 
// //         Visualization(n)(mk.colorSpec, text(mk.label))
// //     }

// //   }

// // }

