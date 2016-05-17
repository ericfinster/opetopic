/**
  * ActiveStableGallery.scala - An Active Stable Gallery
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.syntax.traverse._

import opetopic.ui._

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

      galleryViewport.width = width
      galleryViewport.height = height
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
      boxNesting.foreach(_.render)
      edgeNesting.foreach(_.render)
      renderPanel
    }

    def renderPanel: Unit = {

      val (extCells, intCells) =
        boxNesting.toList.partition(_.isExternal)

      val edges =
        edgeNesting.toList

      panelGroup.children =
        intCells.map(_.cellGroup) ++
          edges.map(_.edgePath) ++
          extCells.map(_.cellGroup)

    }

  }

  //============================================================================================
  // ACTIVE BOXES AND EDGES
  //

  abstract class ActiveBox extends CellBox { thisBox: BoxType => 

    val cellGroup = group
    def element = cellGroup

    val boxRect = {
      val r = rect
      r.r = cornerRadius
      r.strokeWidth = strokeWidth
      r.fill = "white"
      r.stroke = "black"
      r
    }

    def onClick: Unit
    def onMouseOver: Unit
    def onMouseOut: Unit

    boxRect.onClick = { (e : UIMouseEvent) => onClick }
    boxRect.onMouseOver = { (e : UIMouseEvent) => onMouseOver }
    boxRect.onMouseOut = { (e : UIMouseEvent) => onMouseOut }

    def render: Unit = {

      cellGroup.children = Seq(boxRect, labelElement)

      boxRect.x = x ; boxRect.y = y ; boxRect.width = width ; boxRect.height = height

      val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
      val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

      translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

    }

  }

  abstract class ActiveEdge extends CellEdge { thisEdge: EdgeType => 

    val edgePath = {
      val p = path
      p.stroke = "black"
      p.strokeWidth = strokeWidth
      p.fill = "none"
      makeMouseInvisible(p)
      p
    }


    def render: Unit = edgePath.d = pathString

  }

}
