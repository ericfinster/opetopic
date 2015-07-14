/**
  * FXComplexViewer.scala - A JavaFX Complex Viewer Widget
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx

import scala.language.higherKinds
import scala.collection.mutable.ListBuffer

import opetopic._
import opetopic.oldui._
import syntax.tree._
import syntax.complex._
import syntax.nesting._

import javafx.scene.Node

abstract class FXComplexViewer[A[_ <: Nat]](cmplx: FiniteComplex[A])(implicit fxRender: FXRenderable[A]) extends FXViewer[A] 
    with ComplexViewer[A, Double] {

  type MarkerType[N <: Nat] = FXComplexMarker[N]

  type BoxType <: FXBox
  type EdgeType = FXEdge
  type CanvasType = FXCanvas

  val complex : FiniteComplex[MarkerType] =
    initialize(cmplx.value)._1

  //============================================================================================
  // EVENT HANDLERS
  //

  var onSelectAsRoot : IndexedOp[FXComplexMarker] = 
    new IndexedOp[FXComplexMarker] { 
      def apply[N <: Nat](n: N)(mk: FXComplexMarker[N]) = () 
    }

  //============================================================================================
  // MARKER
  //

  class FXComplexMarker[N <: Nat](n: N)(
    val lbl: A[N],
    val address: Address[S[N]],
    val isExternal: Boolean,
    val objectCanvas: FXCanvas,
    val edgeCanvas: FXCanvas
  ) extends FXMarker[N] { thisMarker =>

    def label : A[N] = lbl

    val dim = n

    val box : BoxType = createBox(thisMarker)

    val edge : EdgeType = 
      new FXEdge {
        type Dim = N
        val marker = thisMarker
      }

    def isSelectable : Boolean = true

    var isSelected : Boolean = false
    var isSelectionFace : Boolean = false

    objectCanvas.addBox(box)
    edgeCanvas.addEdge(edge)

  }

  def createBox[N <: Nat](mk: FXComplexMarker[N]) : BoxType 

  def createMarker[N <: Nat](n: N)(
    label: A[N], 
    addr: Address[S[N]],
    isExternal: Boolean,
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : FXComplexMarker[N] =
    new FXComplexMarker(n)(label, addr, isExternal, objCanvas, edgeCanvas)


  def createCanvas : FXCanvas = {
    val c = new FXCanvas { }
    canvases += c
    c
  }

}
