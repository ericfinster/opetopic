/**
  * JsStableEditor.scala - Javascript Stable Editor Implementation
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
// import opetopic.mtl._
// import JsDomFramework._
// import JQuerySemanticUI._

// class JsStableEditor[A: Renderable] {

//   type StableCell = StableEditor[A, JsDomFramework.type]#EditorCell

//   var onSelectAsRoot: StableCell => Unit = { _ => () }

//   //============================================================================================
//   // TAB CLASS
//   //

//   class EditorTab(c: SCardinal[Option[A]], tabName: String) {

//     val editor = new StableEditor[A, JsDomFramework.type](JsDomFramework)(c)

//     editor.onCellClick = 
//       (c: editor.EditorCell) => { }

//     editor.onSelectAsRoot = 
//       (c: StableCell) => onSelectAsRoot(c)

//     editor.layoutWidth = bnds => tabWidth
//     editor.layoutHeight = bnds => tabHeight

//     def refreshDimensions: Unit = {
//       editor.galleryViewport.width = tabWidth
//       editor.galleryViewport.height = tabHeight
//     }

//     editor.renderAll
//     refreshDimensions

//     val tabItem = a(cls := "item", attr("data-tab") := "tab-" ++ tabName)(tabName).render
//     val tab = div(cls := "ui tab", attr("data-tab") := "tab-" ++ tabName)(
//       editor.element.uiElement
//     ).render

//   }

//   //============================================================================================
//   // SHAPE ACTIONS
//   //

//   def doExtrude: Unit = 
//     for { tab <- activeTab } {
//       tab.editor.extrudeSelection
//     }

//   def doDrop: Unit =
//     for { tab <- activeTab } {
//       tab.editor.loopAtSelection
//     }

//   def doSprout: Unit =
//     for { tab <- activeTab } {
//       tab.editor.sproutAtSelection
//     }

//   def doExtract: Unit =
//     for {
//       tab <- activeTab
//       ed = tab.editor
//       pd <- ed.extractSelection
//       // _ = println("Extracted selection: " + pd.map(_.label).toString)
//       ccmplx = ed.complex
//       gpd <- pd.traverse((n: ed.NeutralCell) => {

//         val addr = n.cardinalAddress.complexAddress
//         val codim = ccmplx.dim - n.dim
//         val fa = FaceAddr(codim, addr)

//         for {
//           face <- ccmplx.face(fa)
//         } yield (face : SComplex[ed.EditorCell]).map(_.label)

//       })
//       // _ = println("About to graft")
//       pr <- graft(gpd)({ case (fst, _) => Some(fst) })
//       (web, srcs) = pr
//     } {

//       // println("Finished graft.")

//       // val newComplex : SComplex[Option[A]] =
//       //   web >> SBox(None, srcs) >> SDot(None)

//       newEditor(SCardinal(web, srcs))

//     }

//   // This is a bit of a frankenstein and could be better
//   // organized if the extraction routine above returned
//   // a decent value ....
//   def doCompose: Unit =
//     for {
//       tab <- activeTab
//       ed = tab.editor
//       root <- ed.selectionRoot

//       (cardCmplx: SComplex[ed.EditorCell]) = ed.complex
//       ccmplx = cardCmplx.map(_.label)
//       codim = ccmplx.dim - root.dim
//       ca = root.cardinalAddress.complexAddress
//       fa = FaceAddr(codim, ca)

//       pd <- ed.extractSelection
//       gpd <- pd.traverse((n: ed.NeutralCell) => {

//         val laddr = n.cardinalAddress.complexAddress
//         val lfa = FaceAddr(codim, laddr)

//         ccmplx.face(lfa)

//       })

//       pr <- graft(gpd)({ case (fst, _) => Some(fst) })
//       (web, srcs) = pr

//       contractor = web >> SBox(None, srcs) >> SDot(None)
//       compCmplx <- contractAt(ccmplx, contractor, fa)

//     } {

//       println("Composition complete")

//       SCardinal.fromCardinalComplex(compCmplx) match {
//         case None => println("Error parsing cardinal complex")
//         case Some(card) => newEditor(card)
//       }

//     }

//   //============================================================================================
//   // ACTIONS
//   //

//   def updateLabel(f: Option[A] => Option[A]): Unit = 
//     for {
//       tab <- activeTab
//       root <- tab.editor.selectionRoot
//     } {
//       root.label = f(root.label)
//       refreshEditor
//       // root.selectAsRoot
//     }

//   def rootAction(f: StableCell => Unit): Unit = 
//     for {
//       tab <- activeTab
//       root <- tab.editor.selectionRoot
//     } { f(root) }

//   def withRoot[B](f: StableCell => Option[B]): Option[B] = 
//     for {
//       tab <- activeTab
//       root <- tab.editor.selectionRoot
//       res <- f(root)
//     } yield res

//   def rootFace: Option[SComplex[Option[A]]] =
//     withRoot(_.face)

//   //============================================================================================
//   // EDITOR MANAGEMENT
//   //

//   val tabs: Buffer[EditorTab] = Buffer()

//   var tabCount: Int = 0
//   var activeTab: Option[EditorTab] = None

//   def newEditor: Unit = newEditor(SCardinal[A]())
//   def newEditor(c: SCardinal[Option[A]]) : Unit = {

//     // Name is just the count ...
//     val editorTab = new EditorTab(c, tabCount.toString)
//     tabs += editorTab
//     tabCount += 1
    
//     jQuery(paginationMenu).append(editorTab.tabItem)
//     jQuery(tabPane).append(editorTab.tab)

//     jQuery(editorTab.tabItem).tab(lit(
//       onVisible = (s: String) => { activeTab = Some(editorTab) }
//     ))

//     jQuery(editorTab.tabItem).click()

//   }

//   def refreshEditor: Unit = 
//     for {
//       tab <- activeTab
//     } { tab.editor.renderAll }

//   def closeEditor: Unit =
//     for {
//       tab <- activeTab
//     } {

//       jQuery(tab.tabItem).remove
//       jQuery(tab.tab).remove
//       tabs -= tab

//       if (tabs.length > 0) {
//         jQuery(tabs.head.tabItem).click()
//       }

//     }

//   //============================================================================================
//   // UI ELEMENTS
//   //


//   val paginationMenu = div(cls := "ui pagination menu").render

//   val sidebar = div(cls := "ui inverted visible vertical sidebar menu")(
//     div(cls := "item")(
//       div(cls := "ui input")(input(`type`:= "text", placeholder := "Label ..."))
//     )
//   ).render

//   val tabPane = div(cls := "ui basic segment", style := "background: grey; min-height: 600px; padding-left: 210px").render

//   val container = div(cls := "ui top attached nofocus inverted pushable segment", tabindex := 0)(
//     sidebar, div(cls := "pusher")(tabPane)
//   ).render

//   val bottomMenu =
//     div(cls := "ui bottom attached inverted segment")(
//       div(cls := "ui grid")(
//         div(cls := "four column row")(
//           div(cls := "left floated column")(
//             // button(cls := "ui icon button", id := "sidebar-toggle")(i(cls := "sidebar icon")),
//             paginationMenu
//           ),
//           div(cls := "right floated right aligned column")(
//             button(cls := "ui icon button", onclick := { () => doExtrude })(i(cls := "square outline icon")),
//             button(cls := "ui icon button", onclick := { () => doDrop })(i(cls := "tint icon")),
//             button(cls := "ui icon button", onclick := { () => doSprout })(i(cls := "leaf outline icon")),
//             button(cls := "ui icon button", onclick := { () => doExtract })(i(cls := "cut icon")),

//             button(cls := "ui icon button", onclick := { () => newEditor })(i(cls := "plus icon")),
//             button(cls := "ui icon button", onclick := { () => closeEditor })(i(cls := "minus icon"))
//           )
//         )
//       )
//     ).render

//   val uiElement =
//     div(container, bottomMenu).render


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

//     // jQuery(sidebar).sidebar(lit(context = container, dimPage = false, transition = "overlay")).sidebar("attach events", "#sidebar-toggle")

//     // Install the key handler
//     jQuery(uiElement).keypress(handleKeyEvent(_))

//     jQuery(dom.window).on("resize", () => { resizeInstances })

//     newEditor

//   }

//   def handleKeyEvent(ev: JQueryEventObject): Unit = {
//     ev.which match {
//       case 101 => doExtrude
//       case 100 => doDrop
//       case 115 => doSprout
//       case 120 => doExtract
//       case 99 => doCompose
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
