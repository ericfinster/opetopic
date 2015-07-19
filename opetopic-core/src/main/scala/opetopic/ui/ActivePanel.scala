/**
  * ActivePanel.scala - A panel with support for modification
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import syntax.nesting._

trait HasActivePanels extends HasSelectablePanels { self : ActiveFramework =>

  import isNumeric._

  trait ActivePanel[A, E <: Element, N <: Nat] extends Panel[A, E, N] with SelectablePanel[A, E, N] {

    import config._

    val panelGroup = group
    val element = panelGroup
      
    type BoxType <: ActiveCellBox
    type EdgeType <: ActiveCellEdge

    trait ActiveCellBox extends SelectableBox {

      val boxRect = {
        val r = rect
        r.r = cornerRadius
        r.stroke = "black"
        r.strokeWidth = strokeWidth
        r
      }

      boxRect.onMouseOver = { (e : UIEventType) => boxRect.fill = "blue" }
      boxRect.onMouseOut = { (e : UIEventType) => boxRect.fill = colorHint }

      val boxGroup = group
      val element = boxGroup

      def setHoveredStyle: Unit = boxRect.fill = "blue"
      def setUnhoveredStyle: Unit = boxRect.fill = if (isSelected) "red" else "white"
      def setSelectedStyle: Unit = boxRect.fill = "red"
      def setDeselectedStyle: Unit = boxRect.fill = colorHint

      def render : Unit = {

        // We need a more systematic approach to handling the
        // child tree structure ...
        boxRect.fill = colorHint
        boxGroup.children = Seq(boxRect, labelElement)

        boxRect.x = x ; boxRect.y = y ; boxRect.width = width ; boxRect.height = height

        val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
        val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

        translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

      }

    }

    trait ActiveCellEdge extends CellEdge {

      val edgePath = {
        val p = path
        p.stroke = "black"
        p.strokeWidth = strokeWidth
        p.fill = "none"
        makeMouseInvisible(p)
        p
      }

      val element = edgePath

      def render : Unit = {
        edgePath.d = pathString
      }

    }

  }

  trait ActiveObjectPanel[A, E <: Element] extends ObjectPanel[A, E] {
    self : ActivePanel[A, E, _0] =>

    def bounds: Bounds = {
      val baseBox = boxNesting.baseValue
      Bounds(baseBox.x, baseBox.y, baseBox.width, baseBox.height)
    }

    def refresh: Unit = {
      layoutObjects(boxNesting)
      panelGroup.children = (boxNesting.nodes map (b => { b.render ; b.element }))
    }

  }

  trait ActiveNestingPanel[A, E <: Element, P <: Nat] extends NestingPanel[A, E, P] {
    self : ActivePanel[A, E, S[P]] =>

    import config._

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

    def collectNodes : Unit = {
      val (externalNodes, internalNodes) = boxNesting.nodes.partition(_.isExternal)
      val edgeNodes = edgeNesting.nodes
      panelGroup.children = 
        internalNodes.map(b => { b.render ; b.element }) ++ 
          edgeNodes.map(e => { e.render ; e.element }) ++ 
          externalNodes.map(b => { b.render ; b.element})
    }

    def refresh: Unit = {
      layout(boxNesting, edgeNesting)
      collectNodes
    }

  }

  //============================================================================================
  // SIMPLE ACTIVE PANEL IMPLEMENTATION
  //

  abstract class SimpleActivePanel[A, E <: Element, N <: Nat] extends ActivePanel[A, E, N] {

    type AddressType = Address[S[N]]

    type BoxType = ActiveCellBox
    type EdgeType = ActiveCellEdge

    def cellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) : BoxType =
      new SimpleActiveCellBox(lbl, addr, isExt)

    def cellEdge : EdgeType =
      new SimpleActiveCellEdge

    def generateBoxes(n: N)(nst: Nesting[A, N]) : Nesting[BoxType, N] =
      Nesting.elimWithAddress[A, Nesting[BoxType, N], N](n)(nst)({
        case (a, addr) => Nesting.external(n)(cellBox(a, addr, true))
      })({
        case (a, addr, cn) => Box(cellBox(a, addr, false), cn)
      })

    def seekToAddress(addr: Address[S[N]]) : ShapeM[NestingZipper[BoxType, N]] = 
      boxNesting.seekTo(addr)

    class SimpleActiveCellBox(val label: A, val address: Address[S[N]], val isExternal: Boolean) extends ActiveCellBox {

      val decoration = affixable.decoration(label)
      makeMouseInvisible(labelElement)

    }

    class SimpleActiveCellEdge extends ActiveCellEdge 

  }

  class SimpleActiveObjectPanel[A, E <: Element](val config: PanelConfig, val nesting: Nesting[A, _0])(implicit val affixable: Affixable[A, E])
      extends SimpleActivePanel[A, E, _0] with ActiveObjectPanel[A, E] { 

    def panelDim = Z
    val boxNesting = generateBoxes(nesting.dim)(nesting)
    refresh 

  }

  class SimpleActiveNestingPanel[A, B, E <: Element, P <: Nat](p: P)(
    val config: PanelConfig,
    val nesting: Nesting[A, S[P]], 
    edgeOpt : Option[Nesting[B, P]]
  )(implicit val affixable: Affixable[A, E]) extends SimpleActivePanel[A, E, S[P]] with ActiveNestingPanel[A, E, P] {

    def panelDim = S(p)
    val boxNesting = generateBoxes(nesting.dim)(nesting)

    val edgeNesting : Nesting[EdgeType, P] =
      edgeOpt match {
        case None => reconstructEdges(boxNesting.dim.pred)(boxNesting)
        case Some(et) => connectEdges(et map (_ => cellEdge), boxNesting)
      }

    refresh

  }

  //============================================================================================
  // CONSTRUCTOR
  //

  object ActivePanel {

    @natElim
    def apply[A, E <: Element, N <: Nat](n: N)(nst: Nesting[A, N])(implicit cfg: PanelConfig, r: Affixable[A, E]) : ActivePanel[A, E, N] = {
      case (Z, nst) => new SimpleActiveObjectPanel(cfg, nst)
      case (S(p), nst) => new SimpleActiveNestingPanel(p)(cfg, nst, None)
    }

    def apply[A, E <: Element, N <: Nat](nst: Nesting[A, N])(implicit cfg: PanelConfig, r: Affixable[A, E]) : ActivePanel[A, E, N] = 
      ActivePanel(nst.dim)(nst)

    def apply[A, E <: Element, P <: Nat](nst: Nesting[A, S[P]], et: Nesting[A, P])(implicit cfg: PanelConfig, r: Affixable[A, E]) : ActivePanel[A, E, S[P]] = 
      new SimpleActiveNestingPanel(et.dim)(cfg, nst, Some(et))

  }

}
