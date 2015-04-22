/**
  * CardinalEditor.scala - An editor widget for Opetopic Cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx

import scala.language.higherKinds
import scala.collection.mutable.ListBuffer

import javafx.scene.Node
import javafx.scene.text.Text
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle

import opetopic._
import opetopic.ui._

import TypeDefs._
import Cardinal._

import syntax.complex._

abstract class FXCardinalEditor[A[_ <: Nat]](implicit fxr: FXRenderable[A]) 
    extends FXViewer[({ type L[K <: Nat] = Polarity[Option[A[K]]] })#L] 
    with CardinalEditor[A, Double] {

  type BoxType = FXCardinalBox
  type EdgeType = FXEdge
  type CanvasType = FXCardinalCanvas

  type NeutralBoxType[N <: Nat] <: FXNeutralBox[N]

  type MarkerType[N <: Nat] = FXCardinalMarker[N]
  type NeutralMarkerType[N <: Nat] = FXNeutralMarker[N]
  type PolarizedMarkerType[N <: Nat] = FXPolarizedMarker[N]

  import FXRenderable._

  val labelRenderer : FXRenderable[({ type L[K <: Nat] = Polarity[Option[A[K]]] })#L] =
    polarityIsRenderable[({ type L[K <: Nat] = Option[A[K]] })#L](optIsRenderable[A](fxr))

  var editorState : EditorState = EditorState(Obj(None))

  var onSelectAsRoot : IndexedOp[FXCardinalMarker] = 
    new IndexedOp[FXCardinalMarker] { 
      def apply[N <: Nat](n: N)(mk: FXCardinalMarker[N]) = () 
    }

  //============================================================================================
  // MARKER IMPLEMENTATIONS
  //

  abstract class FXCardinalMarker[N <: Nat] extends FXMarker[N] with CardinalMarker[N] {
    thisMarker =>

    val edge : EdgeType = new FXEdge {
      type Dim = N
      val marker = thisMarker
    }

  }

  class FXNeutralMarker[N <: Nat](
    val dim : N,
    val el : Option[A[N]], 
    val address : CardinalAddress[S[N]],
    val isExternal : Boolean,
    val objectCanvas : FXCardinalCanvas,
    val edgeCanvas : FXCardinalCanvas
  ) extends FXCardinalMarker[N] with NeutralMarker[N] {

    private var markerElement : Option[A[N]] = el

    def label : Polarity[Option[A[N]]] = 
      Neutral(markerElement)

    def element : Option[A[N]] = 
      markerElement

    def element_=(el: Option[A[N]]) : Unit = {
      markerElement = el
      box.refreshLabel
    }

    // Ugly ...
    val nbox : FXNeutralBox[N] = createNeutralBox(this)
    val box : FXCardinalBox = nbox
  
    var isSelected = false
    var isSelectionFace = false
    val isSelectable = true

    cardinalAddress = Some(address)

    objectCanvas.addNeutralBox(nbox)
    edgeCanvas.addEdge(edge)

  }

  class FXPolarizedMarker[N <: Nat](
    val dim : N,
    val lbl : Polarization[Option[A[N]]],
    val isExternal : Boolean,
    val objectCanvas : FXCardinalCanvas,
    val edgeCanvas : FXCardinalCanvas
  ) extends FXCardinalMarker[N] with PolarizedMarker[N] {

    def label : Polarity[Option[A[N]]] = lbl

    // Ugly
    val pbox : FXPolarizedBox[N] = new FXPolarizedBox(this)
    val box : FXCardinalBox = pbox

    val isSelectable = false

    var isSelected = false
    var isSelectionFace = false

    def isPositive : Boolean = 
      label match {
        case Positive() => true
        case _ => false
      }

    objectCanvas.addPolarizedBox(pbox)
    edgeCanvas.addEdge(edge)

  }

  //============================================================================================
  // MARKER CONSTRUCTORS
  //

  def createNeutralMarker[N <: Nat](n: N)(
    element: Option[A[N]],
    addr: CardinalAddress[S[N]],
    isExternal: Boolean,
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : FXNeutralMarker[N] = 
    new FXNeutralMarker(n, element, addr, isExternal, objCanvas, edgeCanvas)

  def createPositiveMarker[N <: Nat](n: N)(
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : FXPolarizedMarker[N] = 
    new FXPolarizedMarker(n, Positive[Option[A[N]]](), false, objCanvas, edgeCanvas)

  def createNegativeMarker[N <: Nat](n: N)(
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : FXPolarizedMarker[N] = 
    new FXPolarizedMarker(n, Negative[Option[A[N]]](), true, objCanvas, edgeCanvas)

  //============================================================================================
  // CANVAS IMPLEMENTATION
  //

  class FXCardinalCanvas extends FXCanvas {

    var positiveBox : Option[FXCardinalBox] = None
    var negativeBox : Option[FXCardinalBox] = None

    def addNeutralBox[N <: Nat](box: FXNeutralBox[N]) : Unit = {

      group.getChildren add box

      for { pos <- positiveBox } { 
        box.toBack
        pos.toBack 
      }

    }

    def addPolarizedBox[N <: Nat](box: FXPolarizedBox[N]) : Unit = {

      if (box.isPositive) {
        positiveBox = Some(box)
      } else {
        negativeBox = Some(box)
      }

      group.getChildren add box

    }

  }

  def createCanvas = {
    val c = new FXCardinalCanvas
    canvases += c
    c
  }

  //============================================================================================
  // BOX IMPLEMENTATIONS
  //

  abstract class FXCardinalBox extends FXBox { 

    var boxLabel : Node = 
      labelRenderer.render(marker.dim)(marker.label)

    def label : Node = 
      boxLabel

    def refreshLabel : Unit = {
      boxLabel = labelRenderer.render(marker.dim)(marker.label)
      pane.getChildren.setAll(label)
    }

    pane.getChildren.add(label)

  }

  def createNeutralBox[N <: Nat](mk : FXNeutralMarker[N]) : FXNeutralBox[N]

  abstract class FXNeutralBox[N <: Nat] extends FXCardinalBox { thisBox : NeutralBoxType[N] =>

    type Dim = N

    override def doHoverStyle = setBackground(genBg(Color.CORNFLOWERBLUE))
    override def doUnhoverStyle = setBackground(genBg(Color.WHITE))
    override def doSelectedStyle = setBackground(genBg(Color.ROYALBLUE))
    override def doUnselectedStyle = setBackground(genBg(Color.WHITE))

    // override def onClick = deselectAll

  }

  class FXPolarizedBox[N <: Nat](mk : FXPolarizedMarker[N]) extends FXCardinalBox {

    type Dim = N

    def marker : MarkerType[Dim] = mk
    def isPositive : Boolean = mk.isPositive

    def color : Color = Color.GAINSBORO

  }

}

