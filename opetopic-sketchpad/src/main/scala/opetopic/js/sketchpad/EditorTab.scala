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

  val editor = CardinalEditor[ConstString]
  // editor.onSelectAsRoot = showBoxProperties

  val uiElement = 
    div(tabindex := 0)(
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

  // jQuery(uiElement).keydown((e : JQueryEventObject) => {
  //   if (e.which == 8) {
  //     e.preventDefault
  //     if (mode == LabelMode) deleteFromLabel
  //   }
  // }).keypress((e : JQueryEventObject) => {
  //   mode match {
  //     case DeformMode => 
  //       e.which match {
  //         case 101 => editor.extrudeSelection
  //         case 100 => editor.extrudeDrop
  //         case 112 => editor.sprout
  //         case _ => ()
  //       }
  //     case LabelMode => {
  //       val c = e.which.toChar
  //       if (c.isLetterOrDigit) appendToLabel(c)
  //     }
  //   }
  // })


}
