/**
  * StableEditor.scala - A Stable Opetopic Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.Buffer

import opetopic._
import opetopic.ui._

class StableEditor[A : Renderable, F <: ActiveFramework](frmwk: F) 
    extends ActiveStableGallery[Polarity[Option[A]], F](frmwk) 
    with SelectableComplex[Polarity[Option[A]]]
    with MutableComplex[Polarity[Option[A]]] { thisEditor => 

  import framework._
  import isNumeric._

  type CellType = EditorCell
  type PanelType = EditorPanel

  val renderer = Renderable[Polarity[Option[A]]]

  //
  //  Visual Options
  //

  var internalPadding : Size = fromInt(400)
  var externalPadding : Size = fromInt(600)
  var leafWidth : Size = fromInt(200)
  var strokeWidth : Size = fromInt(100)
  var cornerRadius : Size = fromInt(200)

  //
  //  Gallery Options
  //

  var width: Size = fromInt(900)
  var height: Size = fromInt(300)
  var minViewX: Option[Size] = None
  var minViewY: Option[Size] = None
  var spacing: Size = fromInt(2000)
  var manageViewport : Boolean = false

  //
  //  Editor Panels
  //

  def panels = editorPanels.toList
  val editorPanels: Buffer[EditorPanel]= {

    // Initialize an empty editor panel
    val posTop = new PositiveCell
    val cell = new NeutralCell(None)
    val posBase = new PositiveCell

    posBase.canopy = Some(SNode(cell, SNode(SLeaf, SLeaf)))
    cell.container = Some(posBase)
    cell.outgoing = Some(posTop)
    posBase.incoming = Some(posTop)
    posTop.target = Some(posBase)
    posTop.sourceTree = posBase.canopy

    Buffer(
      new EditorPanel(posBase), 
      new EditorPanel(posTop)
    )

  }


  class EditorPanel(val baseCell: PositiveCell) extends ActiveStablePanel

  abstract class EditorCell extends ActiveCell 
      with MutableCell
      with SelectableCell {

    def be: BoundedElement
    def labelBounds: Bounds = be.bounds
    def labelElement: Element = be.element

  }

  class NeutralCell(var value: Option[A]) extends EditorCell {
    var be: BoundedElement = renderer.render(framework)(label)
    def label: Polarity[Option[A]] = Neutral(value)
    var isSelected = false
    val canSelect = true

    override def onMouseOver: Unit = {
      boxRect.stroke = "red"
      edgePath.stroke = "red"
    }

    override def onMouseOut: Unit = {
      boxRect.stroke = "black"
      edgePath.stroke = "black"
    }

  }

  abstract class PolarizedCell extends EditorCell {

    val canSelect = false
    val isSelected = false
    def isSelected_=(b: Boolean): Unit = ()
    boxRect.fill = "lightgrey"

    def onMouseOver: Unit = ()
    def onMouseOut: Unit = ()

  }

  class PositiveCell extends PolarizedCell {
    val label: Polarity[Option[A]] = Positive()
    val be: BoundedElement = renderer.render(framework)(label)
  }

  class NegativeCell extends PolarizedCell {
    val label: Polarity[Option[A]] = Negative()
    val be: BoundedElement = renderer.render(framework)(label)
  }

}

