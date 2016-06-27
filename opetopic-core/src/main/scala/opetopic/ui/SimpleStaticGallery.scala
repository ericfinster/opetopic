/**
  * SimpleStaticGallery.scala - A Static Gallery Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

class SimpleStaticGallery[A : Renderable, F <: UIFramework](frmwk: F)(val complex: SComplex[A])
    extends StaticStableGallery[F](frmwk) with ComplexGallery[F] {

  import framework._
  import isNumeric._

  type LabelType = A

  type PanelType = SimpleStaticPanel
  type CellType = SimpleStaticCell
  type SelectionType = SimpleStaticCell

  //
  //  Visual Options
  //

  var internalPadding : Size = fromInt(400)
  var externalPadding : Size = fromInt(600)
  var decorationPadding : Size = fromInt(300)
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

  var firstPanel: Option[Int] = None
  var lastPanel: Option[Int] = None

  val panels: Suite[SimpleStaticPanel] = buildPanels(complex)

  def createPanel(bn: SNesting[CellType], ed: Either[PanelType, SNesting[CellType]]): PanelType = 
    new SimpleStaticPanel(bn, ed)

  def createCell(lbl: LabelType, dim: Int, addr: SAddr, isExternal: Boolean): CellType = 
    new SimpleStaticCell(lbl, dim, addr, isExternal)

  class SimpleStaticCell(val label: A, val dim: Int, val address: SAddr, val isExternal: Boolean) extends StaticCell {

    val cellRendering: CellRendering = 
      implicitly[Renderable[A]].
        render(framework)(label)

  }

  class SimpleStaticPanel(
    val boxNesting: SNesting[BoxType],
    val edgeData: Either[PanelType, SNesting[EdgeType]]
  ) extends StaticPanel with ComplexPanel {

    def dim: Int = boxNesting.baseValue.dim

  }

}
