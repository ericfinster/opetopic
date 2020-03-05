/**
 * CardinalEditorPane.scala - Encapsulate a cardinal editor
 * 
 * @author Eric Finster
 * @version 0.1 
 */

package opetopic.js.ui

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.js._
import opetopic.ui._
import opetopic.mtl._
import JsDomFramework._
import JQuerySemanticUI._

class CardinalEditorPane[A](
  val editor: ActiveStableGallery[JsDomFramework.type]
      with MutableCardinalGallery[JsDomFramework.type]
) extends Component {

  //============================================================================================
  // UI 
  //

  val uiElement =
    div(tabindex := 0, cls := "ui inverted grey nofocus plain segment", style := "margin: 0; padding: 0;")(
      editor.element.uiElement
    ).render

  override def setWidth(w: Int): Unit = {
    super.setWidth(w)
    editor.galleryViewport.width = w
  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)
    editor.galleryViewport.height = h
  }

  editor.layoutWidth = bnds => jQuery(uiElement).width.toInt
  editor.layoutHeight = bnds => jQuery(uiElement).height.toInt
  editor.layoutViewport = bnds => setViewport(bnds)

  //============================================================================================
  // EVENT HANDLERS
  //

  def handleKeyEvent(ev: JQueryEventObject): Unit = {
    ev.which match {
      case 101 => editor.extrudeSelection
      case 100 => editor.loopAtSelection
      case 115 => editor.sproutAtSelection
      case _ => ()
    }
  }

  var xOffset: Double = 0
  var yOffset: Double = 0
  var scale: Double = 0.8

  def setViewport(bnds: Bounds): Bounds = {

    val halfWidth = bnds.width / 2
    val halfHeight = bnds.height / 2

    val centerX = bnds.x + halfWidth;
    val centerY = bnds.y + halfHeight;

    val newHalfWidth = halfWidth / scale
    val newHalfHeight = halfHeight / scale

    val newX = centerX - newHalfWidth + xOffset
    val newY = centerY - newHalfHeight + yOffset

    Bounds(newX.toInt, newY.toInt, (newHalfWidth * 2).toInt, (newHalfHeight * 2).toInt)

  }

  def zoomIn: Unit = {
    scale += 0.1
    editor.renderAll
  }

  def zoomOut: Unit = {
    scale -= 0.1
    editor.renderAll
  }

  //============================================================================================
  // INITIALIZATION
  //

  def initialize: Unit = {

    jQuery(uiElement).keypress(handleKeyEvent(_))
    editor.renderAll

  }

}
