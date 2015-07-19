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

  trait StaticPanel[A, E <: Element, N <: Nat] extends Panel[A, E, N] {

    import config._

    type AddressType = Address[S[N]]

    type BoxType <: StaticCellBox
    type EdgeType <: StaticCellEdge

    def cellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) : BoxType 
    def cellEdge : EdgeType 

    def generateBoxes(n: N)(nst: Nesting[A, N]) : Nesting[BoxType, N] =
      Nesting.elimWithAddress[A, Nesting[BoxType, N], N](n)(nst)({
        case (a, addr) => Nesting.external(n)(cellBox(a, addr, true))
      })({
        case (a, addr, cn) => Box(cellBox(a, addr, false), cn)
      })

    def seekToAddress(addr: Address[S[N]]) : ShapeM[NestingZipper[BoxType, N]] = 
      boxNesting.seekTo(addr)

    def element: Element
    def bounds: Bounds

    trait StaticCellBox extends CellBox {

      def element: Element = {

        val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
        val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

        val locatedLabel = translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

        group(rect(x, y, width, height, cornerRadius, "black", strokeWidth, colorHint), locatedLabel)

      }

    }

    trait StaticCellEdge extends CellEdge {

      def element: Element = {
        path(pathString, "black", strokeWidth, "none")
      }

    }

  }

  trait StaticObjectPanel[A, E <: Element]
      extends StaticPanel[A, E, _0] with ObjectPanel[A, E] {

    import config._

    def element: Element = {
      group (boxNesting.nodes map (_.element) : _*)
    }

    def bounds: Bounds = {
      val baseBox = boxNesting.baseValue
      Bounds(baseBox.x, baseBox.y, baseBox.width, baseBox.height)
    }

  }

  trait StaticNestingPanel[A, E <: Element, P <: Nat]
      extends StaticPanel[A, E, S[P]] with NestingPanel[A, E, P] {

    import config._

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

  }

  //============================================================================================
  // SIMPLE STATIC PANEL IMPLEMENTATION
  //

  abstract class SimpleStaticPanel[A, E <: Element, N <: Nat](val nesting: Nesting[A, N]) 
    extends StaticPanel[A, E, N] {

    val boxNesting : Nesting[BoxType, N] =
      generateBoxes(nesting.dim)(nesting)

    type BoxType = SimpleStaticCellBox
    type EdgeType = SimpleStaticCellEdge

    def cellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) : BoxType =
      new SimpleStaticCellBox(lbl, addr, isExt)

    def cellEdge : EdgeType =
      new SimpleStaticCellEdge

    class SimpleStaticCellBox(val label: A, val address: Address[S[N]], val isExternal: Boolean) 
        extends CellBox with StaticCellBox {

      val decoration = affixable.decoration(label)

    }

    class SimpleStaticCellEdge extends StaticCellEdge 

  }

  class SimpleStaticObjectPanel[A, E <: Element](val config: PanelConfig, nst: Nesting[A, _0])(
    implicit val affixable : Affixable[A, E]
  ) extends SimpleStaticPanel[A, E, _0](nst) with StaticObjectPanel[A, E] {

    def panelDim = Z

    layoutObjects(boxNesting)

  }

  class SimpleStaticNestingPanel[A, B, E <: Element, P <: Nat](p: P)(
    val config: PanelConfig, 
    nst: Nesting[A, S[P]], 
    edgeOpt : Option[Nesting[B, P]]
  )(implicit val affixable: Affixable[A, E]) extends SimpleStaticPanel[A, E, S[P]](nst) with StaticNestingPanel[A, E, P] {

    def panelDim = S(p)

    val edgeNesting : Nesting[EdgeType, P] =
      edgeOpt match {
        case None => reconstructEdges(boxNesting.dim.pred)(boxNesting)
        case Some(et) => connectEdges(et map (_ => cellEdge), boxNesting)
      }

    layout(boxNesting, edgeNesting)

  }

  //============================================================================================
  // CONSTRUCTOR
  //

  object StaticPanel {

    @natElim
    def apply[A, E <: Element, N <: Nat](n: N)(cfg: PanelConfig, nst: Nesting[A, N])(implicit affix: Affixable[A, E]) : StaticPanel[A, E, N] = {
      case (Z, cfg, nst) => new SimpleStaticObjectPanel(cfg, nst)
      case (S(p), xfg, nst) => new SimpleStaticNestingPanel(p)(cfg, nst, None)
    }

    def apply[A, E <: Element, N <: Nat](cfg: PanelConfig, nst: Nesting[A, N])(implicit affix: Affixable[A, E]) : StaticPanel[A, E, N] =
      StaticPanel(nst.dim)(cfg, nst)

    def apply[A, E <: Element, N <: Nat](nst: Nesting[A, N])(implicit cfg: PanelConfig, affix: Affixable[A, E]) : StaticPanel[A, E, N] =
      StaticPanel(nst.dim)(cfg, nst)

    def apply[A, E <: Element, P <: Nat](cfg: PanelConfig, nst: Nesting[A, S[P]], et: Nesting[A, P])(implicit affix: Affixable[A, E]) : StaticPanel[A, E, S[P]] =
      new SimpleStaticNestingPanel(et.dim)(cfg, nst, Some(et))

  }

}
