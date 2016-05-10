/**
  * ActiveStableGallery.scala - Abstract Active Gallery 
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import opetopic.ui._

abstract class ActiveStableGallery[A, F <: ActiveFramework](frmwk: F) 
    extends StableGallery[A, F](frmwk) {

  import framework._
  import isNumeric._

  type PanelType <: ActiveStablePanel
  type CellType <: ActiveCell

  //============================================================================================
  // ACTIVE PANELS
  //

  abstract class ActiveStablePanel extends StablePanel {

    val panelGroup = group
    def element = panelGroup

    def renderPanel: Unit = {

      val (extCells, intCells) =
        baseCell.interiorCells.partition(_.isExternal)

      val edges =
        baseCell.target match {
          case None => List()
          case Some(tgt) => tgt.interiorCells
        }

      panelGroup.children =
        intCells.map(_.cellGroup) ++
      edges.map(_.edgePath) ++
      extCells.map(_.cellGroup)

    }

  }

  //============================================================================================
  // ACTIVE CELLS
  //

  abstract class ActiveCell extends VisualCell { thisCell : CellType =>

    val cellGroup = group
    def element = cellGroup

    val boxRect = {
      val r = rect
      r.r = cornerRadius
      r.strokeWidth = strokeWidth
      r
    }

    boxRect.onMouseOver = { (e : UIMouseEvent) => () }
    boxRect.onMouseOut = { (e : UIMouseEvent) => () }
    boxRect.onClick = { (e : UIMouseEvent) => () }

    val edgePath = {
      val p = path
      p.stroke = "black"
      p.strokeWidth = strokeWidth
      p.fill = "none"
      makeMouseInvisible(p)
      p
    }

    def renderCell: Unit = {

      cellGroup.children = Seq(boxRect, labelElement)

      boxRect.fill = "white"
      boxRect.stroke = "black"

      boxRect.x = x ; boxRect.y = y ; boxRect.width = width ; boxRect.height = height

      val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
      val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

      translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

    }

    def renderEdge : Unit = edgePath.d = pathString

  }

}
