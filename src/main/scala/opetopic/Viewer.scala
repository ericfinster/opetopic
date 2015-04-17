/**
  * Viewer.scala - Abstract Viewer Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds

import scalaz.syntax.monad._

import TypeDefs._

import syntax.tree._
import syntax.complex._

trait Viewer[U] extends Renderer[U] { 

  type BoxType <: ViewerBox
  type EdgeType <: ViewerEdge
  type CanvasType <: ViewerCanvas

  type LabelType[N <: Nat] 
  type MarkerType[N <: Nat] <: ViewerMarker[N]


  //============================================================================================
  // VIEWER CLASSES
  //

  trait ViewerMarker[N <: Nat] extends RenderMarker {

    val label : LabelType[N]

    val objectCanvas : CanvasType
    val edgeCanvas : CanvasType

    def box : BoxType 
    def edge : EdgeType

    var faceComplex : Option[Complex[MarkerType, N]]

  }

  trait ViewerBox {

    type Dim <: Nat

    def marker : MarkerType[Dim]

  }

  trait ViewerEdge {

    type Dim <: Nat

    def marker : MarkerType[Dim]

  }

  trait ViewerCanvas

  def createCanvas : CanvasType
  def displayCanvas(canvas : CanvasType) : Unit

  //============================================================================================
  // RENDER COMPLEX
  //
 
  def renderComplex[N <: Nat](cmplx : Complex[MarkerType, N]) : Unit =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = Complex[MarkerType, N] => Unit

      def caseZero : Out[_0] = {
        case Complex(_, hd) => {
          println("========= Dimension 0 =========")
          renderObjectNesting(hd)
        }
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case Complex(tl, hd) => {

          renderComplex(tl)

          println("========= Dimension " ++ natToInt(tl.length).toString ++  " =========")

          for {
            spine <- tl.headSpine

            leaves = spine map (rm => 
              new LayoutMarker {

                val element = new EdgeStartMarker(rm)
                val rootEdgeMarker = rm
                val external = true

                override def leftInternalMargin = halfLeafWidth
                override def rightInternalMargin = halfLeafWidth

              }
            )

            totalLayout <- renderNesting(hd, leaves)
            baseLayout = Nesting.baseValue(hd)

          } yield {

            import isNumeric._

            // This should probably be a foreach ....
            spine map (rm => { rm.edgeStartY = baseLayout.y - (fromInt(2) * externalPadding) })
            totalLayout.rootEdgeMarker.edgeEndY = baseLayout.rootY + (fromInt(2) * externalPadding)

          }
        }
      }

    })(cmplx.length.pred)(cmplx)

}
