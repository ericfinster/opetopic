/**
 * TabbedCardinalEditor.scala - A Tabbed Cardinal Editor
 * 
 * @author Eric Finster
 * @version 0.1 
 */

package opetopic.js.ui

import scala.collection.mutable.Buffer
import scala.collection.immutable.Queue

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

class TabbedCardinalEditor[A](
  defaultCardinal : SCardinal[Option[A]] = SCardinal[A]()
)(implicit rn: Renderable[A, JsDomFramework.type]) extends Component { thisEditor =>

  type StableCell = SimpleStableEditor[A, JsDomFramework.type]#SimpleCell

  var onSelectAsRoot: StableCell => Unit = { _ => () }
  var onCellShiftClick: StableCell => Unit = { _ => () }
  var onCellCtrlClick: StableCell => Unit = { _ => () }

  //============================================================================================
  // TAB HANDLING
  //

  class EditorTab(val id: String, card: SCardinal[Option[A]]) {

    var xOffset: Double = 0
    var yOffset: Double = 0
    var scale: Double = 0.8
    var undoDepth: Int = 10
    var undoQueue: Queue[SCardinal[Option[A]]] = Queue(card)

    val editor = new SimpleStableEditor[A, JsDomFramework.type](JsDomFramework)(card)
    editor.hoverCofaces = true

    editor.onCellClick =
      (c: editor.SimpleCell) => { }

    editor.onCellShiftClick =
      (c: StableCell) => onCellShiftClick(c)

    editor.onCellCtrlClick =
      (c: StableCell) => onCellCtrlClick(c)
    
    editor.onSelectAsRoot =
      (c: StableCell) => onSelectAsRoot(c)

    // Rendering sets the viewport size to that of the parent
    editor.layoutWidth = bnds => jQuery(thisEditor.uiElement).width.toInt
    editor.layoutHeight = bnds => jQuery(thisEditor.uiElement).height.toInt
    editor.layoutViewport = bnds => setViewport(bnds)

    def pushUndo: Unit = {

      if (undoQueue.length >= undoDepth) {
        undoQueue = undoQueue.dropRight(1)
      }

      val optCard = editor.cardinal.traverseCardinal[Id, Option[A]](_.label)
      undoQueue = optCard +: undoQueue

    }

    def popUndo: Unit =
      if (undoQueue.length > 0) {
        val (last, rest) = undoQueue.dequeue
        undoQueue = rest
        editor.cardinal = last
        editor.renderAll
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

    def setWidth(w: Int): Unit = {
      editor.galleryViewport.width = w
    }

    def setHeight(h: Int): Unit = {
      editor.galleryViewport.height = h
    }
    
    val uiElement =
      div(cls := "ui tab", attr("data-tab") := id)(editor.element.uiElement).render

  }

  val tabs: Buffer[EditorTab] = Buffer()
  var activeTab: Option[EditorTab] = None

  def newTab: Unit =
    newTab(defaultCardinal)

  def newTab(card: SCardinal[Option[A]]): Unit = {

    val tabNo = tabs.length + 1
    val tabId = "tab" ++ tabNo.toString
    val tab = new EditorTab(tabId, card)
    val item = a(cls := "item", attr("data-tab") := tabId, onclick := { () => activeTab = Some(tab) })(tabNo.toString).render

    tabs += tab

    jQuery(tabDiv).append(tab.uiElement)
    tab.editor.renderAll
    
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
        a(cls := "item", onclick := { () => newTab })("New Tab"),
        a(cls := "item", onclick := { () => clearTab })("Reset Tab"),
        div(cls := "ui divider"),
        div(cls := "item", onclick := { () => doExtrude })("Extrude"),
        div(cls := "item", onclick := { () => doLoop })("Drop"),
        div(cls := "item", onclick := { () => doSprout })("Sprout"),
        div(cls := "ui divider"),
        div(cls := "item", onclick := { () => doUndo })("Undo"),
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
    tabs.foreach(_.setWidth(w))
  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)
    tabs.foreach(_.setHeight(h))
  }

  //============================================================================================
  // EVENT HANDLERS
  //

  def doUndo: Unit =
    for { t <- activeTab } {
      t.popUndo
    }

  def doExtrude: Unit =
    for { t <- activeTab } {
      t.pushUndo
      t.editor.extrudeSelection
    }

  def doLoop: Unit =
    for { t <- activeTab } {
      t.pushUndo
      t.editor.loopAtSelection
    }

  def doSprout: Unit =
    for { t <- activeTab } {
      t.pushUndo
      t.editor.sproutAtSelection
    }

  def handleKeyEvent(ev: JQueryEventObject): Unit = {
    ev.which match {
      case 101 => doExtrude
      case 100 => doLoop
      case 115 => doSprout
      case 117 => doUndo
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
    jQuery(dropdown).dropdown(lit(action = "hide"))
    
    newTab

  }

}
