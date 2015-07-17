/**
  * ActivePanel.scala - A panel with support for modification
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import syntax.nesting._

trait ActivePanelFramework[U] { frmwk: ActiveFramework[U] with PanelFramework[U] =>

  import isNumeric._

  // It would be nice to take the initial nesting out of the constructor so that 
  // the memory can be freed.  That is, we should start with an empty panel ....
  abstract class ActivePanel[A, E <: Element, N <: Nat](cfg: PanelConfig)(nst: Nesting[A, N])(implicit r: Affixable[A, E])
    extends Panel[A, E, N](cfg) {

    val panelGroup = group
    val element = panelGroup

    // This will be stateful since we expect
    // active panels to be able to update ...
    var nesting = nst
    var boxNesting = generateBoxes(nst.dim)(nst)
      
    var needsLayout : Boolean = true
    var needsRefresh : Boolean = true

    type BoxType = ActiveCellBox
    type EdgeType = ActiveCellEdge

    def cellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) : BoxType = 
      new ActiveCellBox(lbl, addr, isExt)

    def cellEdge : EdgeType = 
      new ActiveCellEdge

    class ActiveCellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) extends CellBox(lbl, addr, isExt) {

      val boxRect = {
        val r = rect
        r.r = cornerRadius
        r.stroke = "black"
        r.strokeWidth = fullStrokeWidth
        r.fill = colorHint
        r
      }

      val boxGroup = group(boxRect, labelElement)

      val element = boxGroup

      def render : Unit = {

        boxRect.x = x ; boxRect.y = y ; boxRect.width = width ; boxRect.height = height

        val labelXPos = x + width - fullStrokeWidth - internalPadding - labelWidth
        val labelYPos = y + height - fullStrokeWidth - internalPadding - labelHeight

        translate(labelElement, labelXPos - labelBBox.x, labelYPos - labelBBox.y)

      }

    }

    class ActiveCellEdge extends CellEdge {

      val edgePath = {
        val p = path
        p.stroke = "black"
        p.strokeWidth = fullStrokeWidth
        p.fill = "none"
        p
      }

      val element = edgePath

      def render : Unit = {
        edgePath.d = pathString
      }

    }

  }

  class ActiveObjectPanel[A, E <: Element](cfg: PanelConfig)(nst: Nesting[A, _0])(implicit r: Affixable[A, E]) 
      extends ActivePanel[A, E, _0](cfg)(nst) with ObjectPanel[A, E] {

    def bounds: BBox = {
      val baseBox = boxNesting.baseValue
      BBox(baseBox.x, baseBox.y, baseBox.width, baseBox.height)
    }

    layoutObjects(boxNesting)
    panelGroup.children = (boxNesting.nodes map (b => { b.render ; b.element }))

  }

  class ActiveNestingPanel[A, B, E <: Element, P <: Nat](cfg: PanelConfig)(nst: Nesting[A, S[P]], edgeOpt : Option[Nesting[B, P]])(implicit r: Affixable[A, E]) 
      extends ActivePanel[A, E, S[P]](cfg)(nst) with NestingPanel[A, E, P] {

    def bounds: BBox = {
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

      BBox(
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

    def collectNodes : Unit = {
      val (externalNodes, internalNodes) = boxNesting.nodes.partition(_.isExternal)
      val edgeNodes = edgeNesting.nodes
      panelGroup.children = 
        internalNodes.map(b => { b.render ; b.element }) ++ 
          edgeNodes.map(e => { e.render ; e.element }) ++ 
          externalNodes.map(b => { b.render ; b.element})
    }

    layout(boxNesting, edgeNesting)
    collectNodes

  }

  //============================================================================================
  // CONSTRUCTOR
  //

  object ActivePanel {

    @natElim
    def apply[A, E <: Element, N <: Nat](n: N)(nst: Nesting[A, N])(implicit r: Affixable[A, E]) : ActivePanel[A, E, N] = {
      case (Z, nst) => new ActiveObjectPanel(defaultPanelConfig)(nst)
      case (S(p), nst) => new ActiveNestingPanel(defaultPanelConfig)(nst, None)
    }

    def apply[A, E <: Element, N <: Nat](nst: Nesting[A, N])(implicit r: Affixable[A, E]) : ActivePanel[A, E, N] = 
      ActivePanel(nst.dim)(nst)

    def apply[A, E <: Element, P <: Nat](nst: Nesting[A, S[P]], et: Nesting[A, P])(implicit r: Affixable[A, E]) : ActivePanel[A, E, S[P]] = 
      new ActiveNestingPanel(defaultPanelConfig)(nst, Some(et))

  }

}
