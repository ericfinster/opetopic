/**
  * JsComplexEditor.scala - JS routines for complex editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

// package opetopic.js

// import scala.collection.mutable.Buffer

// import org.scalajs.dom
// import org.scalajs.jquery._
// import scalatags.JsDom.all._
// import scala.scalajs.js.Dynamic.{literal => lit}

// import opetopic._
// import opetopic.ui._
// import JsDomFramework._
// import JQuerySemanticUI._

// class JsComplexEditor[A: Renderable] {

//   type GalleryType = ComplexEditor[A, JsDomFramework.type]
//   type CellType = GalleryType#EditorCell

//   var onSelectAsRoot: CellType => Unit = { _ => () }

//   //============================================================================================
//   // TAB CLASS
//   //

//   class EditorTab(c: SComplex[Option[A]]) {

//     val editor = new ComplexEditor[A, JsDomFramework.type](JsDomFramework)(c)

//     editor.onCellClick = 
//       (c: editor.EditorCell) => { }

//     editor.onSelectAsRoot = 
//       (c: CellType) => onSelectAsRoot(c)

//     editor.layoutWidth = bnds => tabWidth
//     editor.layoutHeight = bnds => tabHeight

//     def refreshDimensions: Unit = {
//       editor.galleryViewport.width = tabWidth
//       editor.galleryViewport.height = tabHeight
//     }

//     editor.renderAll
//     refreshDimensions

//   }

//   //============================================================================================
//   // EDITOR MANAGEMENT
//   //

//   val tabs: Buffer[EditorTab] = Buffer()

//   var tabCount: Int = 0
//   var activeTab: Option[EditorTab] = None

//   def newEditor: Unit = newEditor(||(SDot(None)))
//   def newEditor(c: SComplex[Option[A]]) : Unit = {

//     val editorTab = new EditorTab(c)
//     tabs += editorTab
//     tabCount += 1

//     val cntStr = tabCount.toString
//     val tabName = "tab-" ++ cntStr

//     val tabItem = a(cls := "item", attr("data-tab") := tabName)(cntStr).render
//     val tab = div(cls := "ui tab", attr("data-tab") := tabName)(
//       editorTab.editor.element.uiElement
//     ).render

//     jQuery(paginationMenu).append(tabItem)
//     jQuery(tabPane).append(tab)

//     jQuery(tabItem).tab(lit(
//       onVisible = (s: String) => { activeTab = Some(editorTab) }
//     ))

//     jQuery(tabItem).click()

//   }

//   def refreshEditor: Unit = 
//     for {
//       tab <- activeTab
//     } { tab.editor.renderAll }

//   //============================================================================================
//   // UI ELEMENTS
//   //

//   val tabPane = div(cls := "ui middle attached nofocus segment", tabindex := 0, style := "min-height: 350px; background: none").render
//   val paginationMenu = div(cls := "ui pagination menu").render
  
//   val topMenu =
//     div(cls := "ui top attached borderless menu", style := "background: none")(
//       div(cls := "item")(
//         div(cls := "ui primary labeled dropdown icon button")(
//           span(cls := "text")("Shape"), i(cls := "dropdown icon"),
//           div(cls := "menu")(
//             div(cls := "item", style := "min-width: 150px", onclick := { () => () })(span(cls := "description")("e"), "Extrude"),
//             div(cls := "item", onclick := { () => () })(span(cls := "description")("d"), "Drop"),
//             div(cls := "item", onclick := { () => () })(span(cls := "description")("s"), "Sprout"),
//             div(cls := "item", onclick := { () => () })(span(cls := "description")("x"), "Extract")
//           )
//         )
//       )
//     ).render

//   val bottomMenu =
//     div(cls := "ui bottom attached segment", style := "background: none")(
//       div(cls := "ui grid")(
//         div(cls := "four column row")(
//           div(cls := "left floated column")(
//             paginationMenu
//           ),
//           div(cls := "right floated right aligned column")(
//             button(cls := "ui icon button", onclick := { () => newEditor })(i(cls := "add icon"))
//           )
//         )
//       )
//     ).render

//   val uiElement =
//     div(topMenu, tabPane, bottomMenu).render


//   //============================================================================================
//   // UI STATE
//   //

//   var tabWidth : Int = 0
//   var tabHeight : Int = 0

//   //============================================================================================
//   // INITIALIZATION
//   //

//   def initialize: Unit = {

//     // The idea is that we're going to hook the resize event
//     // on the parent element and manage the viewport ourselves..

//     tabWidth = jQuery(tabPane).width.toInt
//     tabHeight = jQuery(tabPane).height.toInt

//     // Install the key handler
//     jQuery(uiElement).keypress(handleKeyEvent(_))

//     jQuery(topMenu).
//       find(".dropdown.button").
//       dropdown(lit(action = "hide"))

//     jQuery(dom.window).on("resize", () => { resizeInstances })

//     newEditor

//   }

//   def handleKeyEvent(ev: JQueryEventObject): Unit = {
//     ev.which match {
//       case 101 => () // doExtrude
//       case 100 => () // doDrop
//       case 115 => () // doSprout
//       case 120 => () // doExtract
//       case 99 => () // doCompose
//       case _ => ()
//     }
//   }

//   def resizeInstances: Unit = {

//     tabWidth = jQuery(tabPane).width.toInt
//     // tabHeight = jQuery(tabPane).height.toInt

//     for {
//       tab <- tabs
//     } { tab.refreshDimensions }

//   }

// }
