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

  class SimpleActiveCell(val label: A) extends ActiveCell {

    val labelBE: BoundedElement =
      implicitly[Renderable[A]].
        render(framework)(label)

    def labelBounds: Bounds = labelBE.bounds
    def labelElement: Element = labelBE.element

  }

  object ComplexImporter extends ComplexBuilder[A, SimpleActiveCell] {

    def newCell(a: A): SimpleActiveCell = new SimpleActiveCell(a)
    def newCell(a: A, d: Nat): SimpleActiveCell = {
      val c = newCell(a)
      c.dim = natToInt(d)
      c
    }

    def registerBaseCell(cell: SimpleActiveCell): Unit = 
      myPanels += SimpleActivePanel(cell)

  }

  object SimpleFactory extends CellFactory[A, SimpleActiveCell] {
    def newCell(a: A, d: Int): SimpleActiveCell = {
      val c = new SimpleActiveCell(a)
      c.dim = d
      c
    }
  }

}
