/**
  * ActiveStableGallery.scala - An Active Stable Gallery
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

abstract class ActiveStableGallery[F <: ActiveFramework](frmwk: F) 
    extends StableGallery[F](frmwk) {

  import framework._
  import isNumeric._

  type BoxType <: ActiveBox
  type EdgeType <: ActiveEdge

  type PanelType <: ActiveStablePanel

  val galleryViewport = viewport
  def element = galleryViewport

  def renderAll: Unit = {

    val (gbnds, els) = panelElementsAndBounds
    panels.foreach(_.renderAll)

    if (manageViewport) {
      galleryViewport.width = width
      galleryViewport.height = height
    }

    galleryViewport.setBounds(gbnds)
    galleryViewport.children = els.toList

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

  trait ActiveBox extends GalleryBox { thisBox: BoxType => 

    // Events
    def onClick: Unit
    def onCtrlClick: Unit

    def onMouseOver: Unit = {
      boxFace.map(bc => {
        bc.foreach(b => b.onHover)
      })
    }

    def onMouseOut: Unit = {
      boxFace.map(bc => {
        bc.foreach(b => b.onUnhover)
      })
    }

    var isHovered: Boolean = false

    def onHover: Unit = {
      isHovered = true
      setFill
      setStroke
    }

    def onUnhover: Unit = {
      isHovered = false
      setFill
      setStroke
    }

    // UI Elements

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

    boxRect.onClick = { (e : UIMouseEvent) => if (e.ctrlKey) onCtrlClick else onClick }
    boxRect.onMouseOver = { (e : UIMouseEvent) => onMouseOver }
    boxRect.onMouseOut = { (e : UIMouseEvent) => onMouseOut }

    def setFill: Unit = { boxRect.fill = colorSpec.fill }
    def setStroke: Unit = { boxRect.stroke = colorSpec.stroke }

    def renderBox: Unit = {

      boxGroup.children = Seq(boxRect, labelElement)

      setFill
      setStroke

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
