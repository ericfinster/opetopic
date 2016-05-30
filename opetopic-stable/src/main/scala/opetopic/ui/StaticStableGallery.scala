/**
  * StaticStableGallery.scala - A Static Gallery for static rendering
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

abstract class StaticStableGallery[F <: UIFramework](frmwk: F) 
    extends StableGallery[F](frmwk) {

  import framework._
  import isNumeric._

  type BoxType <: StaticBox
  type EdgeType <: StaticEdge

  type PanelType <: StaticPanel

  def element: Element = {
    val (gBounds, els) = panelElementsAndBounds
    viewport(width, height, gBounds, els.toList: _*)
  }

  trait StaticBox extends GalleryBox { thisBox : BoxType => 

    def boxElement: Element = {

      val boxRect = rect(
        x, y, width, height, cornerRadius, 
        colorSpec.stroke, strokeWidth, colorSpec.fill
      )

      val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
      val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

      val tl = translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

      group(boxRect, tl)

    }

  }

  trait StaticEdge extends CellEdge { thisEdge : EdgeType => 

    def edgeElement: Element = 
      path(pathString, "black", strokeWidth, "none")

  }

  trait StaticPanel extends StablePanel { thisPanel : PanelType => 

    def element: Element = {

      val (extCells, intCells) =
        boxNesting.toList.partition(_.isExternal)

      val edges =
        if (boxNesting.baseValue.dim > 0)
          edgeNesting.toList
        else List()

      group(
        intCells.map(_.boxElement) ++
          edges.map(_.edgeElement) ++
          extCells.map(_.boxElement) : _*
      )

    }

  }


}


