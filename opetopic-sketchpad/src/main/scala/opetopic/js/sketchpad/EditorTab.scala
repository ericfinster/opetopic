/**
  * EditorTab.scala - A Cardinal Editor Tab
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad
import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.ui._
import opetopic.js._
import syntax.complex._
import syntax.cardinal._
import JsDomFramework._
import JQuerySemanticUI._

class EditorTab {

  val baseConfig: GalleryConfig =
    GalleryConfig(
      panelConfig = defaultPanelConfig,
      width = 1000,
      height = 85,
      spacing = 1500,
      minViewX = Some(60000),
      minViewY = Some(6000),
      spacerBounds = Bounds(0, 0, 600, 600)
    )

  import CellMarker.ActiveInstance._

  val editor = CardinalEditor[CellMarker]
  editor.onSelectAsRoot = (bs: Sigma[editor.CardinalCellBox]) => { activeBox = Some(bs) ; Sketchpad.refreshFacePreview }
  editor.onDeselectAll = () => { activeBox = None }

  val uiElement = 
    div(cls := "nofocus", tabindex := 0)(
      editor.element.uiElement
    ).render


  jQuery(uiElement).keypress((e : JQueryEventObject) => {
    e.which match {
      case 101 => editor.extrudeSelection
      case 100 => editor.extrudeDrop
      case 112 => editor.sprout
      case _ => ()
    }
  })

  var activeBox: Option[Sigma[editor.CardinalCellBox]] = None

}
