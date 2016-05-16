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

  type PolOptA = Polarity[Option[A]]

  val renderer = Renderable[PolOptA]

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

  override def renderAll: Unit = {
    super.renderAll
    refreshAddresses
  }

  def refreshAddresses: Unit = 
    foreachWithAddr({ case (c, a) => c.address = Some(a) })

  def dim: Int = panels.length - 1

  //
  //  Editor Panels
  //

  def panels = editorPanels.toList
  val editorPanels: Buffer[EditorPanel]= {

    // Initialize an empty editor panel
    val posTop = new PositiveCell(1)
    val cell = new NeutralCell(None, 0)
    val posBase = new PositiveCell(0)

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

  //
  //  Cells 
  //

  val cellFactory: CellFactory[PolOptA, EditorCell] = 
    new CellFactory[PolOptA, EditorCell] {
      def newCell(popt: PolOptA, d: Int) = 
        popt match {
          case Neutral(opt) => new NeutralCell(opt, d)
          case Positive() => new PositiveCell(d)
          case Negative() => new NegativeCell(d)
        }

    }

  abstract class EditorCell extends ActiveCell 
      with MutableCell
      with SelectableCell {

    var address: Option[SAddr] = None
    def isPolarized: Boolean 

    def be: BoundedElement
    def labelBounds: Bounds = be.bounds
    def labelElement: Element = be.element

  }

  class NeutralCell(var value: Option[A], val dim: Int) extends EditorCell {
    var be: BoundedElement = renderer.render(framework)(label)
    def label: PolOptA = Neutral(value)
    var isSelected = false
    val canSelect = true
    val isPolarized = false

    override def onSelected: Unit = 
      boxRect.fill = "salmon"

    override def onDeselected: Unit = 
      boxRect.fill = "white"

    override def onClick: Unit = select

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
    val isPolarized = true
    def isSelected_=(b: Boolean): Unit = ()
    boxRect.fill = "lightgrey"

    def onClick: Unit = deselectAll
    def onMouseOver: Unit = ()
    def onMouseOut: Unit = ()

  }

  class PositiveCell(val dim: Int) extends PolarizedCell {
    val label: PolOptA = Positive()
    val be: BoundedElement = renderer.render(framework)(label)
  }

  class NegativeCell(val dim: Int) extends PolarizedCell {
    val label: PolOptA = Negative()
    val be: BoundedElement = renderer.render(framework)(label)
  }

  //============================================================================================
  // MUTABILITY ROUTINES
  //

  def extend: Option[Unit] = 
    for {
      topPanel <- editorPanels.lastOption
    } yield {

      val topBase = topPanel.baseCell
      val topDim = topBase.dim

      val newTop = new PositiveCell(topDim + 1)
      val newNeg = new NegativeCell(topDim)

      val newSrcTr = topBase.sourceDeriv[CellType].plug(newNeg)

      // Setup the new negative cell
      newNeg.container = Some(topBase)
      newNeg.target = topBase.target
      newNeg.outgoing = Some(newTop)
      newNeg.sourceTree = topBase.sourceTree

      // Setup the new top positive cell
      newTop.target = Some(topBase)
      newTop.sourceTree = Some(newSrcTr)

      // Refine the current top cell
      topBase.canopy = Some(newSrcTr)
      topBase.incoming = Some(newTop)

      topBase.target.foreach(_.incoming = Some(newNeg))
      topBase.sourceTree.foreach(_.foreach(_.outgoing = Some(newNeg)))

      val newTopPanel = new EditorPanel(newTop)

      editorPanels += newTopPanel

    }

  def extrudeSelection: Option[Unit] = 
    for {
      root <- selectionRoot
      base <- root.container
      _ <- if (base.isPolarized) { println("Polarization is okay") ; Some(()) } else { println("Polarization is wrong") ; None }
      rootAddr <- root.address
      extAddr <- rootAddr.headOption.map(_.dir)
      _ = println("About to extrude ...")
      _ = if (root.dim > dim - 2) { println("Extending ...") ; extend ; renderAll }
      _ <- base.extrudeAt(Neutral(None), Neutral(None))(extAddr, _.isSelected)
    } yield {
      println("Finished extrusion, rendering")
      deselectAll
      renderAll
    }

}

