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

  // I think we should switch to using the group
  // as the top level element, and having the viewport
  // handled by hte actual ui code upstream
  val galleryGroup = group
  var groupBounds = Bounds(zero,zero,zero,zero)

  val galleryViewport = viewport
  def element = galleryViewport

  def renderAll: Unit = {

    val (gbnds, els) = panelElementsAndBounds
    panels.foreach(_.renderAll)

    galleryGroup.children = els.toList
    groupBounds = gbnds

    galleryViewport.width = layoutWidth(gbnds)
    galleryViewport.height = layoutHeight(gbnds)
    galleryViewport.setBounds(layoutViewport(gbnds))
    galleryViewport.children = Seq(galleryGroup)

  }

  var hoverCofaces: Boolean = false

  //============================================================================================
  // EVENT HANDLERS
  //

  var onCellClick: SelectionType => Unit = { _ => () }
  var onCellCtrlClick: SelectionType => Unit = { _ => () }
  var onCellShiftClick: SelectionType => Unit = { _ => () }
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
        boxNesting.toList.filter(_.isVisible).partition(_.isExternal)

      val edges =
        if (boxNesting.baseValue.dim > 0)
          edgeNesting.toList.filter(_.isVisible)
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
    def onCtrlClick: Unit = { onCellCtrlClick(thisCell) }
    def onShiftClick: Unit = { select ; onCellShiftClick(thisCell) }

    // Hover implementations
    def doHoverFaces: Unit = {
      boxFace.map(bc => {
        bc.foreach(b => b.onHover)
      })
    }

    def doHoverCofaces: Unit = {
      val bc = boxComplex
      val codim = bc.dim - dim

      bc.traverseCofaces[Unit](codim, b => b == thisCell)({
        case Left(b) => Some(())
        case Right(b) => Some(b.onHover)
      })
    }

    def doUnhoverFaces: Unit = {
      boxFace.map(bc => {
        bc.foreach(b => b.onUnhover)
      })
    }

    def doUnhoverCofaces: Unit = {
      val bc = boxComplex
      val codim = bc.dim - dim

      bc.traverseCofaces[Unit](codim, b => b == thisCell)({
        case Left(b) => Some(())
        case Right(b) => Some(b.onUnhover)
      })
    }

    def onMouseOver: Unit = {
      if (hoverCofaces)
        doHoverCofaces

      doHoverFaces
      onHoverEdge
    }

    def onMouseOut: Unit = {
      if (hoverCofaces) 
        doUnhoverCofaces

      doUnhoverFaces
      onUnhoverEdge
    }

    var isHovered: Boolean = false

    def onHover: Unit = {
      thisGallery.onHover(thisCell)
      isHovered = true
      setFill
      // setStroke
    }

    def onUnhover: Unit = {
      thisGallery.onUnhover(thisCell)
      isHovered = false
      setFill
      // setStroke
    }

    def onSelected: Unit = {
      setFill
      // setStroke
    }

    def onDeselected: Unit = {
      setFill
      // setStroke
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

    boxRect.onClick = { (e : UIMouseEvent) =>
      if (e.ctrlKey) onCtrlClick
      else if (e.shiftKey) onShiftClick
      else onClick }

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

    def onHoverEdge: Unit = {
      edgePath.stroke = colorSpec.edgeHovered
    }

    def onUnhoverEdge: Unit = {
      edgePath.stroke = "black"
    }

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
