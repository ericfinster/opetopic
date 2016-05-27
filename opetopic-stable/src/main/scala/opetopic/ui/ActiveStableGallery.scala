/**
  * ActiveStableGallery.scala - An Active Stable Gallery
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

abstract class ActiveStableGallery[A, F <: ActiveFramework](frmwk: F) 
    extends StableGallery[A, F](frmwk) {

  import framework._
  import isNumeric._

  type BoxType <: ActiveBox
  type EdgeType <: ActiveEdge

  type PanelType <: ActiveStablePanel

  val galleryViewport = viewport
  def element = galleryViewport

  override def layout: Option[Bounds] = {
    val bo = super.layout

    for { bnds <- bo } yield {

      if (manageViewport) {
        galleryViewport.width = width
        galleryViewport.height = height
      }

      galleryViewport.setBounds(bnds)

      bnds

    }
  }

  def renderAll: Unit = 
    for { _ <- layout } {
      panels.foreach(_.renderAll)
      galleryViewport.children = panels.map(_.element).toList
    }

  //============================================================================================
  // ACTIVE PANELS
  //

  abstract class ActiveStablePanel extends StablePanel {

    val panelGroup = group
    def element = panelGroup

    def renderAll: Unit = {
      boxNesting.foreach(_.renderBox)
      edgeNesting.foreach(_.renderEdge)
      renderPanel
    }

    def renderPanel: Unit = {

      val (extCells, intCells) =
        boxNesting.toList.partition(_.isExternal)

      val edges =
        if (boxNesting.baseValue.dim > 0)
          edgeNesting.toList
        else List()

      panelGroup.children =
        intCells.map(_.boxGroup) ++
          edges.map(_.edgePath) ++
          extCells.map(_.boxGroup)

    }

  }

  //============================================================================================
  // ACTIVE BOXES AND EDGES
  //

  trait ActiveBox extends CellBox { thisBox: BoxType => 

    def label: A
    def dim: Int
    def address: SAddr

    def cellRendering: CellRendering

    def labelBounds: Bounds = cellRendering.boundedElement.bounds
    def labelElement: Element = cellRendering.boundedElement.element

    def colorSpec: ColorSpec = 
      cellRendering.colorSpec

    val boxGroup = group
    def element = boxGroup

    val boxRect = {
      val r = rect
      r.r = cornerRadius
      r.strokeWidth = strokeWidth
      r.fill = "white"
      r.stroke = "black"
      r
    }

    def onClick: Unit
    def onCtrlClick: Unit
    def onMouseOver: Unit
    def onMouseOut: Unit

    boxRect.onClick = { (e : UIMouseEvent) => if (e.ctrlKey) onCtrlClick else onClick }
    boxRect.onMouseOver = { (e : UIMouseEvent) => onMouseOver }
    boxRect.onMouseOut = { (e : UIMouseEvent) => onMouseOut }

    def renderBox: Unit = {

      boxGroup.children = Seq(boxRect, labelElement)

      boxRect.fill = colorSpec.fill
      boxRect.stroke = colorSpec.stroke

      boxRect.x = x ; boxRect.y = y ; boxRect.width = width ; boxRect.height = height

      val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
      val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

      translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

    }

  }

  trait ActiveEdge extends CellEdge { thisEdge: EdgeType => 

    val edgePath = {
      val p = path
      p.stroke = "black"
      p.strokeWidth = strokeWidth
      p.fill = "none"
      makeMouseInvisible(p)
      p
    }

    def renderEdge: Unit = edgePath.d = pathString

  }

}
