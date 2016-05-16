/**
  * SimpleActiveGallery.scala - A Simple Gallery Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.Buffer

import opetopic._
import opetopic.ui._

class SimpleActiveGallery[A : Renderable, F <: ActiveFramework](frmwk: F) 
    extends ActiveStableGallery[A, F](frmwk) {

  import framework._
  import isNumeric._

  type CellType = SimpleActiveCell
  type PanelType = SimpleActivePanel

  val myPanels: Buffer[SimpleActivePanel] = Buffer.empty
  def panels = myPanels.toList

  var onCellClick: ActiveCell => Unit = { _ => () }

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

  abstract class SimpleActivePanel extends ActiveStablePanel

  object SimpleActivePanel {
    def apply(cell: SimpleActiveCell) : SimpleActivePanel = 
      new SimpleActivePanel { def baseCell = cell }
  }

  class SimpleActiveCell(val label: A, val dim: Int) extends ActiveCell {

    val labelBE: BoundedElement =
      implicitly[Renderable[A]].
        render(framework)(label)

    makeMouseInvisible(labelElement)

    def labelBounds: Bounds = labelBE.bounds
    def labelElement: Element = labelBE.element

    def onClick: Unit = onCellClick(this)

    def onMouseOver: Unit = {
      boxRect.stroke = "red"
      edgePath.stroke = "red"
    }

    def onMouseOut: Unit = {
      boxRect.stroke = "black"
      edgePath.stroke = "black"
    }

  }

  object ComplexImporter extends ComplexBuilder[A, SimpleActiveCell] {

    def newCell(a: A): SimpleActiveCell = new SimpleActiveCell(a, 0)
    def newCell(a: A, d: Nat): SimpleActiveCell = 
      new SimpleActiveCell(a, natToInt(d))

    def registerBaseCell(cell: SimpleActiveCell): Unit = 
      myPanels += SimpleActivePanel(cell)

  }

  object SimpleFactory extends CellFactory[A, SimpleActiveCell] {
    def newCell(a: A, d: Int): SimpleActiveCell = 
      new SimpleActiveCell(a, d)
  }

}
