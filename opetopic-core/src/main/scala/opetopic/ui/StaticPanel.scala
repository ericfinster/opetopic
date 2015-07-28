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

  trait StaticPanel[A, N <: Nat] extends Panel[A, N] {

    import config._

    type BoxType <: StaticCellBox[A, N] { type BoxAddressType = PanelAddressType }
    type EdgeType <: StaticCellEdge[A, N]

    def cellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) : BoxType 

    def generateBoxes(n: N)(nst: Nesting[A, N]) : Nesting[BoxType, N] =
      Nesting.elimWithAddress[A, Nesting[BoxType, N], N](n)(nst)({
        case (a, addr) => Nesting.external(n)(cellBox(a, addr, true))
      })({
        case (a, addr, cn) => Box(cellBox(a, addr, false), cn)
      })

    def seekToAddress(addr: Address[S[N]]) : ShapeM[NestingZipper[BoxType, N]] = 
      boxNesting.seekTo(addr)


  }

  trait StaticCellBox[A, N <: Nat] extends CellBox[A, N] {

    import panel.config._

    def element: Element = {

      val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
      val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

      val locatedLabel = translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

      group(rect(x, y, width, height, cornerRadius, "black", strokeWidth, colorHint), locatedLabel)

    }

  }

  trait StaticCellEdge[A, N <: Nat] extends CellEdge[A, N] {

    import panel.config._

    def element: Element = {
      path(pathString, "black", strokeWidth, "none")
    }

  }

  trait StaticObjectPanel[A] extends StaticPanel[A, _0] with ObjectPanel[A] {

    import config._

    def element: Element = {
      group (boxNesting.nodes map (_.element) : _*)
    }

    def bounds: Bounds = {
      val baseBox = boxNesting.baseValue
      Bounds(baseBox.x, baseBox.y, baseBox.width, baseBox.height)
    }

  }

  trait StaticNestingPanel[A, P <: Nat] extends StaticPanel[A, S[P]] with NestingPanel[A, P] {

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

  abstract class SimpleStaticPanel[A, N <: Nat](val nesting: Nesting[A, N]) 
    extends StaticPanel[A, N] {

    val boxNesting : Nesting[BoxType, N] =
      generateBoxes(nesting.dim)(nesting)

    type PanelAddressType = Address[S[N]]
    type BoxType = SimpleStaticCellBox[A, N]
    type EdgeType = SimpleStaticCellEdge[A, N]

    def cellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) : BoxType =
      new SimpleStaticCellBox(this, lbl, addr, isExt)

    def cellEdge : EdgeType =
      new SimpleStaticCellEdge(this)

  }

  class SimpleStaticCellBox[A, N <: Nat](
    val panel: SimpleStaticPanel[A, N],
    val label: A, 
    val address: Address[S[N]], 
    val isExternal: Boolean
  ) extends CellBox[A, N] with StaticCellBox[A, N] {

    type BoxAddressType = Address[S[N]]

    val decoration = panel.affixable.decoration(label)
    def nestingAddress = address

  }

  class SimpleStaticCellEdge[A, N <: Nat](
    val panel: SimpleStaticPanel[A, N]
  ) extends StaticCellEdge[A, N]

  class SimpleStaticObjectPanel[A](val config: PanelConfig, nst: Nesting[A, _0])(
    implicit val affixable : Affixable[A]
  ) extends SimpleStaticPanel[A, _0](nst) with StaticObjectPanel[A] {

    def panelDim = Z

    layoutObjects(boxNesting)

  }

  class SimpleStaticNestingPanel[A, B, P <: Nat](p: P)(
    val config: PanelConfig, 
    nst: Nesting[A, S[P]], 
    edgeOpt : Option[Nesting[B, P]]
  )(implicit val affixable: Affixable[A]) extends SimpleStaticPanel[A, S[P]](nst) with StaticNestingPanel[A, P] {

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
    def apply[A, N <: Nat](n: N)(cfg: PanelConfig, nst: Nesting[A, N])(implicit affix: Affixable[A]) : StaticPanel[A, N] = {
      case (Z, cfg, nst) => new SimpleStaticObjectPanel(cfg, nst)
      case (S(p), xfg, nst) => new SimpleStaticNestingPanel(p)(cfg, nst, None)
    }

    def apply[A, N <: Nat](cfg: PanelConfig, nst: Nesting[A, N])(implicit affix: Affixable[A]) : StaticPanel[A, N] =
      StaticPanel(nst.dim)(cfg, nst)

    def apply[A, N <: Nat](nst: Nesting[A, N])(implicit cfg: PanelConfig, affix: Affixable[A]) : StaticPanel[A, N] =
      StaticPanel(nst.dim)(cfg, nst)

    def apply[A, P <: Nat](cfg: PanelConfig, nst: Nesting[A, S[P]], et: Nesting[A, P])(implicit affix: Affixable[A]) : StaticPanel[A, S[P]] =
      new SimpleStaticNestingPanel(et.dim)(cfg, nst, Some(et))

  }

}
