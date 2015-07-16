/**
  * InteractivePanel.scala - A panel with support for user interaction
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._

trait InteractivePanelFramework[U] { frmwk: InteractiveFramework[U] with PanelFramework[U] =>

  import isNumeric._

  abstract class InteractivePanel[A, E <: ElementType, N <: Nat](cfg: PanelConfig)(implicit r: Affixable[A, E])
    extends Panel[A, E, N](cfg) {

    // Here are some flags for telling whether the
    // structure is dirty with respect to layout or
    // with respect to node structure ...

    var needsLayout : Boolean = true
    var needsRebuild : Boolean = true

    // The main thing here is that the cell box and edge guys get overriden so 
    // that they can pass events back to the panel

    type BoxType <: InteractiveCellBox
    type EdgeType <: InteractiveCellEdge

    abstract class InteractiveCellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) extends CellBox(lbl, addr, isExt) {

      val boxRect = rect(x, y, width, height, cornerRadius, "black", fullStrokeWidth, colorHint)
      val boxGroup = group(boxRect, labelElement)
      val element = boxGroup

      // Now, with this set up, we in fact have references to the actual visual elements
      // (or some shim thereof) which are representing this semantic entity.

    }

    abstract class InteractiveCellEdge extends CellEdge {

      val edgePath = path("", fullStrokeWidth)
      val element = edgePath

    }

  }

}
