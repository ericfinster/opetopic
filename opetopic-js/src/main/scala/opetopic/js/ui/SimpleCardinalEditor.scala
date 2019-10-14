/**
 * SimpleCardinalEditor.scala - A Simple Cardinal Editor
 * 
 * @author Eric Finster
 * @version 0.1 
 */

package opetopic.js.ui

import scala.collection.mutable.Buffer

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

class SimpleCardinalEditor[A: Renderable](c : SCardinal[Option[A]] = SCardinal[A]()) extends Component {

  type StableCell = StableEditor[A, JsDomFramework.type]#EditorCell

  var onSelectAsRoot: StableCell => Unit = { _ => () }

  var xOffset: Double = 0
  var yOffset: Double = 0
  var scale: Double = 0.8

  val editor = new StableEditor[A, JsDomFramework.type](JsDomFramework)(c)

  //============================================================================================
  // UI ELEMENTS
  //
  
  val dropdown =
    button(cls := "ui grey dropdown icon button", style := "position: absolute; left: 10px; top: 10px;")(
      i(cls := "chevron circle down icon"),
      div(cls := "menu")(
        div(cls := "item", onclick := { () => editor.cardinal = c ; editor.renderAll })("New"),
        div(cls := "item", onclick := { () => editor.extrudeSelection })("Extrude"),
        div(cls := "item", onclick := { () => editor.loopAtSelection })("Drop"),
        div(cls := "item", onclick := { () => editor.sproutAtSelection })("Sprout"),
        div(cls := "ui divider"),
        div(cls := "item", onclick := { () => zoomIn })("Zoom in"),
        div(cls := "item", onclick := { () => zoomOut })("Zoom out")
      )
    ).render
    
  val uiElement =
    div(tabindex := 0, cls := "ui inverted grey nofocus plain segment", style := "margin: 0; padding: 0;")(
      dropdown,
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

    editor.onCellClick =
      (c: editor.EditorCell) => { }

    editor.onSelectAsRoot =
      (c: StableCell) => onSelectAsRoot(c)

    // Rendering sets the viewport size to that of the parent
    editor.layoutWidth = bnds => jQuery(uiElement).width.toInt
    editor.layoutHeight = bnds => jQuery(uiElement).height.toInt
    editor.layoutViewport = bnds => setViewport(bnds)

    editor.renderAll

    // Install the key handler
    jQuery(uiElement).keypress(handleKeyEvent(_))
    jQuery(dropdown).dropdown()

  }

}
