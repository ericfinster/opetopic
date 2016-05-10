/**
  * ActiveStablePanel.scala - Abstract Active Panel
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import opetopic.ui._

// abstract class ActiveStablePanel[A, F <: ActiveFramework](frmwk: F) extends StablePanel[A, F](frmwk) {

//   import framework._
//   import isNumeric._

//   override type CellType <: ActivePanelCell

//   val panelGroup = group
//   def element = panelGroup

//   def renderPanel: Unit = {

//     val (extCells, intCells) =
//       baseCell.interiorCells.partition(_.isExternal)

//     val edges =
//       baseCell.target match {
//         case None => List()
//         case Some(tgt) => tgt.interiorCells
//       }

//     panelGroup.children =
//       intCells.map(_.cellGroup) ++
//         edges.map(_.edgePath) ++
//         extCells.map(_.cellGroup)

//   }

//   abstract class ActivePanelCell extends PanelCell { thisCell : CellType => 

//     val cellGroup = group
//     def element = cellGroup

//     val boxRect = {
//       val r = rect
//       r.r = cornerRadius
//       r.strokeWidth = strokeWidth
//       r
//     }

//     boxRect.onMouseOver = { (e : UIMouseEvent) => () }
//     boxRect.onMouseOut = { (e : UIMouseEvent) => () }
//     boxRect.onClick = { (e : UIMouseEvent) => () }

//     val edgePath = {
//       val p = path
//       p.stroke = "black"
//       p.strokeWidth = strokeWidth
//       p.fill = "none"
//       makeMouseInvisible(p)
//       p
//     }

//     def renderCell: Unit = {

//       cellGroup.children = Seq(boxRect, labelElement)

//       boxRect.fill = "white"
//       boxRect.stroke = "black"

//       boxRect.x = x ; boxRect.y = y ; boxRect.width = width ; boxRect.height = height

//       val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
//       val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

//       translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

//     }

//     def renderEdge : Unit = edgePath.d = pathString


//   }

// }
