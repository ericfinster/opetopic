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

  trait ActivePanel[A, N <: Nat] extends Panel[A, N] with SelectablePanel[A, N] {

    import config._

    val panelGroup = group
    def element : Element = panelGroup
      
    type BoxType <: ActiveCellBox[A, N] { type BoxAddressType = PanelAddressType }
    type EdgeType <: ActiveCellEdge[A, N]

    def refresh : Unit

  }

  trait ActiveCellBox[A, N <: Nat] extends SelectableBox[A, N] {

    import panel.config._

    val boxRect = {
      val r = rect
      r.r = cornerRadius
      r.stroke = "black"
      r.strokeWidth = strokeWidth
      r
    }

    boxRect.onMouseOver = { (e : UIMouseEvent) => setHoveredStyle }
    boxRect.onMouseOut = { (e : UIMouseEvent) => setUnhoveredStyle }
    boxRect.onClick = { (e : UIMouseEvent) => onClick }

    val boxGroup = group
    val element = boxGroup

    def onClick: Unit = ()

    def setHoveredStyle: Unit = boxRect.hover 
    def setUnhoveredStyle: Unit = boxRect.unhover 
    def setSelectedStyle: Unit = boxRect.select 
    def setDeselectedStyle: Unit = boxRect.deselect 

    def render : Unit = {

      // We need a more systematic approach to handling the
      // child tree structure ...
      boxRect.addClass(classString)
      boxGroup.children = Seq(boxRect, labelElement)

      boxRect.x = x ; boxRect.y = y ; boxRect.width = width ; boxRect.height = height

      val labelXPos = x + width - strokeWidth - internalPadding - labelWidth
      val labelYPos = y + height - strokeWidth - internalPadding - labelHeight

      translate(labelElement, labelXPos - labelBounds.x, labelYPos - labelBounds.y)

    }

  }

  trait ActiveCellEdge[A, N <: Nat] extends CellEdge[A, N] {

    import panel.config._

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

  trait ActiveObjectPanel[A] extends ActivePanel[A, _0] with ObjectPanel[A] {

    def bounds: Bounds = {
      val baseBox = boxNesting.baseValue
      Bounds(baseBox.x, baseBox.y, baseBox.width, baseBox.height)
    }

    def refresh: Unit = {
      layoutObjects(boxNesting)
      panelGroup.children = (boxNesting.nodes map (b => { b.render ; b.element }))
    }

  }

  trait ActiveNestingPanel[A, P <: Nat] extends ActivePanel[A, S[P]] with NestingPanel[A, P] {

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

  abstract class SimpleActivePanel[A, N <: Nat] extends ActivePanel[A, N] {

    type PanelAddressType = Address[S[N]]

    type BoxType = SimpleActiveCellBox[A, N]
    type EdgeType = SimpleActiveCellEdge[A, N]

    def cellBox(lbl: A, addr: Address[S[N]], isExt: Boolean) : BoxType =
      new SimpleActiveCellBox(this, lbl, addr, isExt)

    def cellEdge : EdgeType =
      new SimpleActiveCellEdge(this)

    def panelWidth: Size = fromInt(150)
    def panelHeight: Size = fromInt(150)

    var onBoxClicked : SimpleActiveCellBox[A, N] => Unit = { _ => () }

    def generateBoxes(n: N)(nst: Nesting[A, N]) : Nesting[BoxType, N] =
      Nesting.elimWithAddress[A, Nesting[BoxType, N], N](n)(nst)({
        case (a, addr) => Nesting.external(n)(cellBox(a, addr, true))
      })({
        case (a, addr, cn) => Box(cellBox(a, addr, false), cn)
      })

    def seekToAddress(addr: Address[S[N]]) : ShapeM[NestingZipper[BoxType, N]] = 
      boxNesting.seekTo(addr)

    val panelViewport = viewport
    override def element = panelViewport

    def setupViewport : Unit = {

      val panelBounds = bounds
      val viewportBounds = 
        Bounds(
          panelBounds.x - fromInt(200),
          panelBounds.y - fromInt(200),
          panelBounds.width + fromInt(400),
          panelBounds.height + fromInt(400)
        )

      panelViewport.width = panelWidth
      panelViewport.height = panelHeight
      panelViewport.setBounds(viewportBounds)
      panelViewport.children = Seq(panelGroup)

    }

  }

  class SimpleActiveCellBox[A, N <: Nat](
    val panel: SimpleActivePanel[A, N], 
    val label: A, 
    val address: Address[S[N]], 
    val isExternal: Boolean
  ) extends ActiveCellBox[A, N] { thisBox =>

    type PanelType = SimpleActivePanel[A, N]
    type BoxAddressType = Address[S[N]]

    val decoration = panel.affixable.decoration(label)
    makeMouseInvisible(labelElement)
    def nestingAddress = address

    override def onClick: Unit = {
      panel.onBoxClicked(thisBox)
    }

  }

  class SimpleActiveCellEdge[A, N <: Nat](val panel: SimpleActivePanel[A, N]) 
      extends ActiveCellEdge[A, N]


  class SimpleActiveObjectPanel[A](val config: PanelConfig, val nesting: Nesting[A, _0])(implicit val affixable: Affixable[A])
      extends SimpleActivePanel[A, _0] with ActiveObjectPanel[A] { 

    val panelDim = Z
    val boxNesting = generateBoxes(nesting.dim)(nesting)

    override def refresh: Unit = {
      super.refresh
      setupViewport
    }

    refresh 

  }

  class SimpleActiveNestingPanel[A, B, P <: Nat](p: P)(
    val config: PanelConfig,
    val nesting: Nesting[A, S[P]], 
    edgeOpt : Option[Nesting[B, P]]
  )(implicit val affixable: Affixable[A]) extends SimpleActivePanel[A, S[P]] with ActiveNestingPanel[A, P] {

    val panelDim = S(p)
    val boxNesting = generateBoxes(nesting.dim)(nesting)

    val edgeNesting : Nesting[EdgeType, P] =
      edgeOpt match {
        case None => reconstructEdges(boxNesting.dim.pred)(boxNesting)
        case Some(et) => connectEdges(et map (_ => cellEdge), boxNesting)
      }

    override def refresh: Unit = {
      super.refresh
      setupViewport
    }

    refresh

  }

  //============================================================================================
  // CONSTRUCTOR
  //

  object ActivePanel {

    @natElim
    def apply[A, N <: Nat](n: N)(nst: Nesting[A, N])(implicit cfg: PanelConfig, r: Affixable[A]) : SimpleActivePanel[A, N] = {
      case (Z, nst) => new SimpleActiveObjectPanel(cfg, nst)
      case (S(p), nst) => new SimpleActiveNestingPanel(p)(cfg, nst, None)
    }

    def apply[A, N <: Nat](nst: Nesting[A, N])(implicit cfg: PanelConfig, r: Affixable[A]) : SimpleActivePanel[A, N] = 
      ActivePanel(nst.dim)(nst)

    def apply[A, P <: Nat](nst: Nesting[A, S[P]], et: Nesting[A, P])(implicit cfg: PanelConfig, r: Affixable[A]) : SimpleActivePanel[A, S[P]] = 
      new SimpleActiveNestingPanel(et.dim)(cfg, nst, Some(et))

  }

}
