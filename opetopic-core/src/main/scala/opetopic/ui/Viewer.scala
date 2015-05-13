/**
  * Viewer.scala - Abstract Viewer Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.language.higherKinds
import scala.collection.mutable.ListBuffer

import scalaz.syntax.monad._

import opetopic._
import TypeDefs._

import syntax.tree._
import syntax.complex._
import syntax.nesting._

trait Viewer[A[_ <: Nat], U] extends Renderer[U] { 

  type MarkerType[N <: Nat] <: ViewerMarker[N]

  type BoxType <: ViewerBox
  type EdgeType <: ViewerEdge
  type CanvasType <: ViewerCanvas

  def complex : FiniteComplex[MarkerType]

  def labelComplex : FiniteComplex[A] = 
    complex.value map (new ~~>[MarkerType, A] {
      def apply[N <: Nat](mk: MarkerType[N]) : A[N] = 
        mk.label
    })

  //============================================================================================
  // EVENT HANDLERS
  //

  var onSelectAsRoot : IndexedOp[MarkerType]

  //============================================================================================
  // VIEWER CLASSES
  //

  trait ViewerMarker[N <: Nat] extends RenderMarker {

    def dim : N
    def address : Address[S[N]]

    def label : A[N]

    def objectCanvas : CanvasType
    def edgeCanvas : CanvasType

    def box : BoxType 
    def edge : EdgeType


    // The fact that this is an "option" is really detestable ...
    var faceComplex : Option[Complex[MarkerType, N]] = None 

    def labelComplex : Option[Complex[A, N]] = 
      for {
        fc <- faceComplex
      } yield {
        fc map (new ~~>[MarkerType, A] {
          def apply[N <: Nat](mk: MarkerType[N]) : A[N] = 
            mk.label
        })
      }

    def isSelectable : Boolean

    var isSelected : Boolean
    var isSelectionFace : Boolean

    def select : Unit = 
      if (isSelectable) {
        for {
          fc <- faceComplex
          mk <- fc
        } {
          mk.isSelectionFace = true
          mk.box.doSelectedStyle
        }
        isSelected = true
      }

    def deselect : Unit = 
      if (isSelectable) {
        for {
          fc <- faceComplex
          mk <- fc
        } {
          mk.isSelectionFace = false
          mk.box.doUnselectedStyle
        }
        isSelected = false
      }

    def hover : Unit = 
      for {
        fc <- faceComplex
        mk <- fc
      } {
        if (! mk.isSelectionFace)
          mk.box.doHoverStyle
      }

    def unhover : Unit = 
      for {
        fc <- faceComplex
        mk <- fc
      } {
        if (! mk.isSelectionFace)
          mk.box.doUnhoverStyle
      }

  }

  trait ViewerElement {

    type Dim <: Nat

    def render : Unit
    def marker : MarkerType[Dim]

    def doHoverStyle : Unit
    def doUnhoverStyle : Unit
    def doSelectedStyle : Unit
    def doUnselectedStyle : Unit

    def onMouseEnter : Unit = marker.hover
    def onMouseExit : Unit = marker.unhover
    def onMouseClick : Unit = selectAsRoot(marker)
    def onMouseCtrlClick : Unit = select(marker)
    def onMouseDoubleClick : Unit = ()
    def onMouseRightClick : Unit = ()

  }

  trait ViewerBox extends ViewerElement
  trait ViewerEdge extends ViewerElement
  trait ViewerCanvas

  def createCanvas : CanvasType
  def displayCanvas(canvas : CanvasType) : Unit

  //============================================================================================
  // RENDER VIEWER
  //

  def render : ShapeM[Unit] = {
    val cmplx = complex

    for {
      _ <- renderComplex[cmplx.N](cmplx.value)
    } yield {
      for {
        mk <- cmplx
      } {
        mk.box.render
        mk.edge.render
      }
    }

  }

  //============================================================================================
  // RENDER COMPLEX
  //
 
  def renderComplex[N <: Nat](cmplx : Complex[MarkerType, N]) : ShapeM[Unit] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = Complex[MarkerType, N] => ShapeM[Unit]

      def caseZero : Out[_0] = {
        case Complex(_, hd) => {
          // println("========= Dimension 0 =========")
          renderObjectNesting(hd)
          succeed(())
        }
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case Complex(tl, hd) => {
          for {

            _ <- renderComplex(tl)

            // _ = println("========= Dimension " ++ natToInt(tl.length).toString ++  " =========")

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

            // Set the positions of incoming edges

            for { rm <- spine } {
              rm.edgeStartY = baseLayout.y - (fromInt(2) * externalPadding)
            }

            // Set the position of the outgoing edge

            totalLayout.rootEdgeMarker.edgeEndY = baseLayout.rootY + (fromInt(2) * externalPadding)

            // println("Render ok ...")
          }
        }
      }

    })(cmplx.length.pred)(cmplx)

  //============================================================================================
  // SELECTION ROTUINES
  //

  sealed trait Selection {
    type Dim <: Nat
    val dim : Dim
    val root : MarkerType[Dim]
    val companions : ListBuffer[MarkerType[Dim]]
  }

  object Selection {

    def apply[N <: Nat](mk: MarkerType[N]) : Selection = 
      new Selection {
        type Dim = N
        val dim = mk.dim
        val root = mk
        val companions = ListBuffer.empty[MarkerType[Dim]]
      }

  }

  var selection : Option[Selection] = None

  def deselectAll : Unit = {
    for {
      sel <- selection
      _ = sel.root.deselect
      marker <- sel.companions
    } {
      marker.deselect
    }

    selection = None
  }

  def selectAsRoot[N <: Nat](marker : MarkerType[N]) : Unit = 
    if (marker.isSelectable) {
      deselectAll
      marker.select
      onSelectAsRoot(marker.dim)(marker)
      selection = Some(Selection(marker))
    }

  def select[N <: Nat](marker : MarkerType[N]) : Unit =
    if (marker.isSelectable) {
      selection match {
        case None => selectAsRoot(marker)
        case Some(sel) => {

          import Nats._
          import Lte._

          matchNatPair(marker.dim, sel.dim) match {
            case None => selectAsRoot(marker)
            case Some(ev) => {

              val curComplex = complex

              for {
                diff <- fromOpt(
                  diffOpt(marker.dim, curComplex.n)
                )
                nst = curComplex.getNesting(diff)
                zipper <- nst seekTo marker.address
              } yield {

                import scalaz.-\/
                import scalaz.\/-

                import scala.collection.mutable.ListBuffer

                val candidates : ListBuffer[MarkerType[N]] = 
                  ListBuffer()

                Nesting.predecessorWhich(zipper)(mk => { 
                  candidates += mk
                  mk.isSelected 
                }) match {
                  case -\/(_) => selectAsRoot(marker)
                  case \/-((fcs, _)) => {
                    for {
                      mk <- candidates.init
                    } {
                      mk.select
                      sel.companions += rewriteNatIn(ev)(mk)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

}
