/**
  * SimpleActiveGallery.scala - A Simple Gallery Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.Buffer

import opetopic.ui._

trait Renderable[A] {
  def render(frmwk: UIFramework)(a: A) : frmwk.BoundedElement
}

class SimpleActiveGallery[A : Renderable, F <: ActiveFramework](frmwk: F) 
    extends ActiveStableGallery[A, F](frmwk) {

  import framework._
  import isNumeric._

  type PanelType = SimpleActivePanel
  type CellType = SimpleActiveCell

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

  class SimpleActivePanel extends ActiveStablePanel {

    def baseCell: SimpleActiveCell = ???

  }

  class SimpleActiveCell(val label: A) extends ActiveCell {

    val labelBE: BoundedElement =
      implicitly[Renderable[A]].
        render(framework)(label)

    def labelBounds: Bounds = labelBE.bounds
    def labelElement: Element = labelBE.element

  }

}
