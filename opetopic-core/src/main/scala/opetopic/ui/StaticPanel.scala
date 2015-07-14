/**
  * StaticPanel.scala - A Static Panel
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import syntax.nesting._

trait StaticPanelFramework[U] { frmwk: RenderingFramework[U] with PanelFramework[U] =>

  import isNumeric._

  abstract class StaticPanel[A, E <: ElementType, N <: Nat](cfg: PanelConfig)(nst: Nesting[A, N])(implicit r: Renderable[A, E])
      extends Panel[A, E, N](cfg)(nst) {

    val boxNesting : Nesting[BoxType, N] = 
      generateBoxes(nst.dim)(nst)

    type BoxType = StaticCellBox
    type EdgeType = StaticCellEdge

    def cellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) : BoxType = 
      new StaticCellBox(lbl, addr, isExt)

    def cellEdge : EdgeType = 
      new StaticCellEdge

    class StaticCellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) extends CellBox(lbl, addr, isExt) {

      def element: ElementType = {

        val labelXPos = x + width - fullStrokeWidth - internalPadding - labelWidth
        val labelYPos = y + height - fullStrokeWidth - internalPadding - labelHeight

        val locatedLabel = translate(labelElement, labelXPos - labelBBox.x, labelYPos - labelBBox.y)

        group(rect(x, y, width, height, cornerRadius, fullStrokeWidth), locatedLabel)

      }

    }

    class StaticCellEdge extends CellEdge {

      def element: ElementType = {
        path(pathString, fullStrokeWidth)
      }

    }

  }

  class StaticObjectPanel[A, E <: ElementType](cfg: PanelConfig)(nst: Nesting[A, _0])(implicit r: Renderable[A, E])
    extends StaticPanel[A, E, _0](cfg)(nst) with ObjectPanel[A, E] {

    def element: ElementType = {
      group (boxNesting.nodes map (_.element) : _*)
    }

    layoutObjects(boxNesting)

  }

  class StaticNestingPanel[A, E <: ElementType, P <: Nat](cfg: PanelConfig)(nst: Nesting[A, S[P]], edgeOpt : Option[Nesting[A, P]])(implicit r: Renderable[A, E])
    extends StaticPanel[A, E, S[P]](cfg)(nst) with NestingPanel[A, E, P] {

    def element: ElementType = {

      val (externalNodes, internalNodes) = boxNesting.nodes.partition(_.isExternal)
      val edgeNodes = edgeNesting.nodes

      val elements = internalNodes.map(_.element) ++ edgeNodes.map(_.element) ++ externalNodes.map(_.element)

      group(elements : _*)

    }

    val edgeNesting : Nesting[EdgeType, P] = 
      edgeOpt match {
        case None => reconstructEdges(boxNesting.dim.pred)(boxNesting)
        case Some(et) => connectEdges(et, boxNesting)
      }


    layout(boxNesting, edgeNesting)

  }

  //============================================================================================
  // CONSTRUCTOR
  //

  object StaticPanel {

    @natElim
    def apply[A, E <: ElementType, N <: Nat](n: N)(nst: Nesting[A, N])(implicit r: Renderable[A, E]) : StaticPanel[A, E, N] = {
      case (Z, nst) => new StaticObjectPanel(defaultPanelConfig)(nst)
      case (S(p), nst) => new StaticNestingPanel(defaultPanelConfig)(nst, None)
    }

    def apply[A, E <: ElementType, N <: Nat](nst: Nesting[A, N])(implicit r: Renderable[A, E]) : StaticPanel[A, E, N] = 
      StaticPanel(nst.dim)(nst)

    def apply[A, E <: ElementType, P <: Nat](nst: Nesting[A, S[P]], et: Nesting[A, P])(implicit r: Renderable[A, E]) : StaticPanel[A, E, S[P]] = 
      new StaticNestingPanel(defaultPanelConfig)(nst, Some(et))

  }

}
