/**
  * StaticPanel.scala - A Panel whose contents are fixed
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import syntax.tree._
import syntax.nesting._

trait HasStaticPanels extends HasPanels { self: UIFramework =>

  import isNumeric._

  abstract class StaticPanel[A, E <: Element, N <: Nat](val nesting: Nesting[A, N]) extends Panel[A, E, N] {

    import config._

    val boxNesting : Nesting[BoxType, N] =
      generateBoxes(nesting.dim)(nesting)

    type BoxType = StaticCellBox
    type EdgeType = StaticCellEdge

    def cellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) : BoxType =
      new StaticCellBox(lbl, addr, isExt)

    def cellEdge : EdgeType =
      new StaticCellEdge

    def element: Element
    def bounds: Bounds

    class StaticCellBox(val label: A, val address: Address[S[N]], val isExternal: Boolean) extends CellBox {

      val (labelElement, labelBounds, colorHint) = {
        val dec = affixable.decoration(label)
        (dec.boundedElement.element, dec.boundedElement.bounds, dec.colorHint)
      }

      def element: Element = {

        val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
        val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

        val locatedLabel = translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

        group(rect(x, y, width, height, cornerRadius, "black", strokeWidth, colorHint), locatedLabel)

      }

    }

    class StaticCellEdge extends CellEdge {

      def element: Element = {
        path(pathString, "black", strokeWidth, "none")
      }

    }

  }

  class StaticObjectPanel[A, E <: Element](val config: PanelConfig, nst: Nesting[A, _0])(
    implicit val affixable : Affixable[A, E]
  ) extends StaticPanel[A, E, _0](nst) with ObjectPanel[A, E] {

    import config._

    def panelDim = Z

    def element: Element = {
      group (boxNesting.nodes map (_.element) : _*)
    }

    def bounds: Bounds = {
      val baseBox = boxNesting.baseValue
      Bounds(baseBox.x, baseBox.y, baseBox.width, baseBox.height)
    }

    layoutObjects(boxNesting)

  }

  class StaticNestingPanel[A, B, E <: Element, P <: Nat](p: P)(
    val config: PanelConfig, 
    nst: Nesting[A, S[P]], 
    edgeOpt : Option[Nesting[B, P]]
  )(implicit val affixable: Affixable[A, E]) extends StaticPanel[A, E, S[P]](nst) with NestingPanel[A, E, P] {

    import config._

    def panelDim = S(p)

    def element: Element = {

      val (externalNodes, internalNodes) = boxNesting.nodes.partition(_.isExternal)
      val edgeNodes = edgeNesting.nodes

      val elements = internalNodes.map(_.element) ++ edgeNodes.map(_.element) ++ externalNodes.map(_.element)

      group(elements : _*)

    }

    def bounds: Bounds = {
      val baseBox = boxNesting.baseValue

      val (panelX, panelWidth) =
        boxNesting match {
          case Dot(box, _) => {

            val allEdges : List[EdgeType] = edgeNesting.nodes

            val (minX, maxX) = (allEdges foldLeft (box.x, box.x + box.width))({
              case ((curMin, curMax), edge) =>
                (isOrdered.min(curMin, edge.edgeStartX), isOrdered.max(curMax, edge.edgeStartX))
            })

            (minX, maxX - minX)
          }
          case Box(box, _) => (box.x, box.width)
        }

      Bounds(
        panelX,
        baseBox.y - (fromInt(2) * externalPadding),
        panelWidth,
        baseBox.height + (fromInt(4) * externalPadding)
      )
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
    def apply[A, E <: Element, N <: Nat](n: N)(cfg: PanelConfig, nst: Nesting[A, N])(implicit affix: Affixable[A, E]) : StaticPanel[A, E, N] = {
      case (Z, cfg, nst) => new StaticObjectPanel(cfg, nst)
      case (S(p), xfg, nst) => new StaticNestingPanel(p)(cfg, nst, None)
    }

    def apply[A, E <: Element, N <: Nat](cfg: PanelConfig, nst: Nesting[A, N])(implicit affix: Affixable[A, E]) : StaticPanel[A, E, N] =
      StaticPanel(nst.dim)(cfg, nst)

    def apply[A, E <: Element, N <: Nat](nst: Nesting[A, N])(implicit cfg: PanelConfig, affix: Affixable[A, E]) : StaticPanel[A, E, N] =
      StaticPanel(nst.dim)(cfg, nst)

    def apply[A, E <: Element, P <: Nat](cfg: PanelConfig, nst: Nesting[A, S[P]], et: Nesting[A, P])(implicit affix: Affixable[A, E]) : StaticPanel[A, E, S[P]] =
      new StaticNestingPanel(et.dim)(cfg, nst, Some(et))

  }

}
