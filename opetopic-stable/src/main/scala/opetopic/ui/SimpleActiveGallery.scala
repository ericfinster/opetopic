/**
  * SimpleActiveGallery.scala - A Simple Gallery Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.collection.mutable.Buffer

import opetopic._

class SimpleActiveGallery[A : Renderable, F <: ActiveFramework](frmwk: F)(val complex: SComplex[A]) 
    extends ActiveStableGallery[F](frmwk) with ComplexGallery[F] {

  import framework._
  import isNumeric._

  type LabelType = A

  type PanelType = SimpleActivePanel
  type CellType = SimpleActiveCell

  var onCellClick: ActiveBox => Unit = { _ => () }

  //
  //  Visual Options
  //

  var internalPadding : Size = fromInt(400)
  var externalPadding : Size = fromInt(600)
  var leafWidth : Size = fromInt(200)
  var strokeWidth : Size = fromInt(100)
  var cornerRadius : Size = fromInt(200)

  //
  //  Gallery Options
  //

  var width: Size = fromInt(900)
  var height: Size = fromInt(300)
  var minViewX: Option[Size] = None
  var minViewY: Option[Size] = None
  var spacing: Size = fromInt(2000)
  var manageViewport : Boolean = false

  val panels : Suite[PanelType] = buildPanels(complex)

  def createPanel(bn: SNesting[CellType], ed: Either[PanelType, SNesting[CellType]]): PanelType = 
    new SimpleActivePanel(bn, ed)

  def createCell(lbl: LabelType, dim: Int, addr: SAddr, isExternal: Boolean): CellType = 
    new SimpleActiveCell(lbl, dim, addr, isExternal)

  class SimpleActivePanel(
    val boxNesting: SNesting[BoxType],
    val edgeData: Either[PanelType, SNesting[EdgeType]]
  ) extends ActiveStablePanel with ComplexPanel {
    def dim: Int = boxNesting.baseValue.dim
  }

  class SimpleActiveCell(val label: A, val dim: Int, val address: SAddr, val isExternal: Boolean)
      extends ActiveBox 
      with ActiveEdge {

    val cellRendering: CellRendering = 
      implicitly[Renderable[A]].
        render(framework)(label)

    makeMouseInvisible(labelElement)

    def onClick: Unit = onCellClick(this)
    def onCtrlClick: Unit = ()

    def onMouseOver: Unit = ()
    def onMouseOut: Unit = ()

    def onHover: Unit = ()
    def onUnhover: Unit = ()

  }

}
