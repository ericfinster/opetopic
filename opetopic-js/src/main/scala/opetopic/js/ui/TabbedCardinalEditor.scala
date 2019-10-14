/**
 * TabbedCardinalEditor.scala - A Tabbed Cardinal Editor
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

class TabbedCardinalEditor[A: Renderable](
  defaultCardinal : SCardinal[Option[A]] = SCardinal[A]()
) extends Component {

  type StableCell = StableEditor[A, JsDomFramework.type]#EditorCell

  var onSelectAsRoot: StableCell => Unit = { _ => () }
  var onCellShiftClick: StableCell => Unit = { _ => () }

  //============================================================================================
  // TAB HANDLING
  //

  class EditorTab(val id: String) {

    var xOffset: Double = 0
    var yOffset: Double = 0
    var scale: Double = 0.8

    val editor = new StableEditor[A, JsDomFramework.type](JsDomFramework)(defaultCardinal)

    editor.onCellClick =
      (c: editor.EditorCell) => { }

    editor.onCellShiftClick =
      (c: StableCell) => onCellShiftClick(c)

    editor.onSelectAsRoot =
      (c: StableCell) => onSelectAsRoot(c)

    // Rendering sets the viewport size to that of the parent
    editor.layoutWidth = bnds => jQuery(uiElement).width.toInt
    editor.layoutHeight = bnds => jQuery(uiElement).height.toInt
    editor.layoutViewport = bnds => setViewport(bnds)
    
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

    val uiElement =
      div(cls := "ui tab", attr("data-tab") := id)(editor.element.uiElement).render

  }

  val tabs: Buffer[EditorTab] = Buffer()
  var activeTab: Option[EditorTab] = None

  def newTab: Unit = {

    val tabNo = tabs.length + 1
    val tabId = "tab" ++ tabNo.toString
    val tab = new EditorTab(tabId)
    val item = a(cls := "item", attr("data-tab") := tabId, onclick := { () => activeTab = Some(tab) })(tabNo.toString).render

    tabs += tab

    jQuery(tabDiv).append(tab.uiElement)
    tab.editor.renderAll

    // Set the default width and height
    tab.editor.galleryViewport.width = jQuery(uiElement).width.toInt
    tab.editor.galleryViewport.height = jQuery(uiElement).height.toInt
    
    jQuery(paginator).append(item)
    jQuery(item).tab()
    jQuery(item).click()
    
  }

  def clearTab: Unit =
    for { t <- activeTab } {
      t.editor.cardinal = defaultCardinal
      t.editor.renderAll
    }

  //============================================================================================
  // UI ELEMENTS
  //
  
  val dropdown =
    button(cls := "ui grey dropdown icon button", style := "position: absolute; left: 10px; top: 10px;")(
      i(cls := "chevron circle down icon"),
      div(cls := "menu")(
        div(cls := "item", onclick := { () => newTab })("New Tab"),
        div(cls := "item", onclick := { () => clearTab })("Reset Tab"),
        div(cls := "ui divider"),
        div(cls := "item", onclick := { () => doExtrude })("Extrude"),
        div(cls := "item", onclick := { () => doLoop })("Drop"),
        div(cls := "item", onclick := { () => doSprout })("Sprout"),
        div(cls := "ui divider"),
        div(cls := "item", onclick := { () => zoomIn })("Zoom in"),
        div(cls := "item", onclick := { () => zoomOut })("Zoom out")
      )
    ).render

  val tabDiv =
    div().render

  val paginator =
    div(cls := "ui pagination menu", style := "position: absolute; left: 10px; bottom: 10px;").render
  
  val uiElement =
    div(tabindex := 0, cls := "ui inverted grey nofocus plain segment", style := "margin: 0; padding: 0;")(
      dropdown, tabDiv, paginator
    ).render

  override def setWidth(w: Int): Unit = {
    super.setWidth(w)
    for { t <- tabs } {
      t.editor.galleryViewport.width = w
    }
  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)
    for { t <- tabs } {
      t.editor.galleryViewport.height = h
    }
  }

  //============================================================================================
  // EVENT HANDLERS
  //

  def doExtrude: Unit =
    for { t <- activeTab } { t.editor.extrudeSelection }

  def doLoop: Unit =
    for { t <- activeTab } { t.editor.loopAtSelection }

  def doSprout: Unit =
    for { t <- activeTab } { t.editor.sproutAtSelection }

  def handleKeyEvent(ev: JQueryEventObject): Unit = {
    ev.which match {
      case 101 => doExtrude
      case 100 => doLoop
      case 115 => doSprout
      case _ => ()
    }
  }

  def zoomIn: Unit =
    for { t <- activeTab } {
      t.scale += 0.1
      t.editor.renderAll
    }

  def zoomOut: Unit =
    for { t <- activeTab } {
      t.scale -= 0.1
      t.editor.renderAll
    }

  //============================================================================================
  // INITIALIZATION
  //

  def initialize: Unit = {

    // Install the key handler
    jQuery(uiElement).keypress(handleKeyEvent(_))
    jQuery(dropdown).dropdown()
    newTab

  }

}
