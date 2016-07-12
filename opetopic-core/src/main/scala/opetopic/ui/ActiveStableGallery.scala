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
    extends StableGallery[F](frmwk) with SelectableGallery { thisGallery => 

  import framework._
  import isNumeric._

  type CellType <: ActiveCell
  type PanelType <: ActiveStablePanel

  val galleryViewport = viewport
  def element = galleryViewport

  def renderAll: Unit = {

    val (gbnds, els) = panelElementsAndBounds
    panels.foreach(_.renderAll)

    galleryViewport.width = layoutWidth(gbnds)
    galleryViewport.height = layoutHeight(gbnds)
    galleryViewport.setBounds(layoutViewport(gbnds))
    galleryViewport.children = els.toList

  }

  //============================================================================================
  // EVENT HANDLERS
  //

  var onCellClick: SelectionType => Unit = { _ => () }
  var onHover: SelectionType => Unit = { _ => () }
  var onUnhover: SelectionType => Unit = { _ => () }

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
          edges.map(_.edgeGroup) ++
          extCells.map(_.boxGroup)

    }

  }

  //============================================================================================
  // ACTIVE CELLS
  //

  abstract class ActiveCell extends GalleryCell with SelectableCell { thisCell: CellType with SelectionType => 

    // Events
    def onClick: Unit = { selectAsRoot ; onCellClick(thisCell) }
    def onCtrlClick: Unit = { select }

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
      thisGallery.onHover(thisCell)
      isHovered = true
      setFill
      setStroke
    }

    def onUnhover: Unit = {
      thisGallery.onUnhover(thisCell)
      isHovered = false
      setFill
      setStroke
    }

    def onSelected: Unit = {
      setFill
      setStroke
    }

    def onDeselected: Unit = {
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

    def setFill: Unit = {
      if (isSelected) {
        boxRect.fill = colorSpec.fillSelected
      } else if (isHovered) {
        boxRect.fill = colorSpec.fillHovered
      } else {
        boxRect.fill = colorSpec.fill
      }
    }

    def setStroke: Unit = {
      if (isSelected) {
        boxRect.stroke = colorSpec.strokeSelected
      } else if (isHovered) {
        boxRect.stroke = colorSpec.strokeHovered
      } else {
        boxRect.stroke = colorSpec.stroke
      }
    }

    def renderBox: Unit = {

      makeMouseInvisible(labelElement)

      boxGroup.children = Seq(boxRect, labelElement)

      setFill
      setStroke

      boxRect.x = x ; boxRect.y = y ; boxRect.width = width ; boxRect.height = height

      val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
      val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

      translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

    }

    //============================================================================================
    // EDGE ROTUINES
    //

    val edgePath = {
      val p = path
      p.stroke = "black"
      p.strokeWidth = strokeWidth
      p.fill = "none"
      makeMouseInvisible(p)
      p
    }

    val edgeGroup = group

    def renderEdge: Unit = {

      edgePath.d = pathString

      val decs = 
        edgeDecorations.map((dm: DecorationMarker) => {
          translate(dm.be.element, dm.rootX - half(dm.be.bounds.width), dm.rootY)
          dm.be.element
        })

      edgeGroup.children = edgePath +: decs.toSeq

    }

  }

}
