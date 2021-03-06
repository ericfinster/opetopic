/**
  * Studio.scala - The opetopic studio 
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.studio

import scala.collection.mutable.Buffer
import scala.concurrent.Future

import scala.scalajs.js
import scala.scalajs.js.timers._
import org.scalajs.jquery._
import org.scalajs.dom
import org.scalajs.dom.Element
import scalatags.JsDom.all._
import js.Dynamic.{literal => lit}

import scalajs.concurrent.JSExecutionContext.Implicits.queue

import opetopic._
import opetopic.ui._
import opetopic.js._
import opetopic.js.ui._
import opetopic.mtl._

import JsDomFramework._
import JQuerySemanticUI._

object Studio {

  val editor = new TabbedCardinalEditor[SimpleMarker]()
  val viewer = new SimpleViewer[Option[SimpleMarker]]

  val faceViewer = new SimpleViewer[Option[SimpleMarker]]
  val linkViewer = new SimpleViewer[Option[SimpleMarker]]

  type EditorCell = editor.StableCell
  type ViewerCell = viewer.CellType

  editor.onSelectAsRoot = (c: EditorCell) => { onEditorSelect(c) }
  editor.onCellCtrlClick = (c: EditorCell) => { onShowCell(c) }
  viewer.onSelectAsRoot = (c: ViewerCell) => { onViewerSelect(c) }

  // UI Elements

  val viewerPane =
    new FixedBottomPane(
      viewer,
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;")(
          div(cls := "item")(
            div(cls := "ui action input")(
              input(id := "sketch-name-input", `type` := "text", placeholder := "Name ...", onchange := { () => onSaveSketch }),
              button(cls := "ui button")("Save")
            )
          ),
          a(cls := "item", onclick := { () => onPaste })(i(cls := "edit outline icon"), "Paste"),
          a(cls := "item", onclick := { () => onEditFace })(i(cls := "edit outline icon"), "Edit Face"),
          // a(cls := "item", onclick := { () => onRefresh })(i(cls := "star icon"), "Refresh"),
          div(cls := "right menu")(
            div(cls := "item")(
              div(cls := "ui action input")(
                input(id := "download-input", `type` := "text", placeholder := "Filename ...", onchange := { () => onDownloadSketch }),
                button(cls := "ui button", onclick := { () => onDownloadSketch })("Download")
              )
            )
          )
        ).render)
    )

  val editorPane =
    new FixedBottomPane(
      editor,
      PlainComponent(
        div(cls := "ui inverted menu", style := "margin-top: 0; border-radius: 0;")(
          div(cls := "item")(
            div(cls := "ui input")(input(`type` := "text", id := "label-input", placeholder := "Label ..."))
          ),
          div(cls := "ui dropdown item")(
            div(cls := "text")("Auto Label"),
            i(cls := "dropdown icon"),
            div(cls := "menu")(
              div(cls := "header")("Type"),
              div(cls := "item", onclick := { () => autoLabel(false) })("Numeric"),
              div(cls := "item", onclick := { () => autoLabel(true) })("Alphabetic")
            )
          )
          // div(cls := "right menu")(
          //   div(cls := "item")(
          //     div(cls := "ui labeled icon button", onclick := { () => onFollowToggle })(i(cls := "check icon", id := "follow-icon"), "Follow Selection")
          //   )
          //   // a(cls := "item", onclick := { () => zoomIn })(i(cls := "zoom in icon"), "Zoom In"),
          //   // a(cls := "item", onclick := { () => zoomOut })(i(cls := "zoom out icon"), "Zoom Out")
          // )
        ).render)
    )


  val flagList = div(cls := "ui fluid inverted vertical menu").render
  val flagTab = new Tab("flag-tab",
    PlainComponent(div(cls := "ui inverted segment",
      style := "padding: 0; margin: 0; overflow-y: auto; overflow-x: hidden;")(flagList).render), true)

  val syntaxPre = pre().render
  val syntaxTab = new Tab("syntax-tab", PlainComponent(div(cls := "ui inverted segment", style := "padding: 0; margin: 0; overflow-x: auto;")(syntaxPre).render))

  val infoPane = new TabPane(flagTab, syntaxTab)

  val inspectorPane = 
    new FixedBottomPane(
      new HorizontalSplitPane(
        new VerticalSplitPane(faceViewer, linkViewer),
        infoPane
        // 
      ),
      PlainComponent(div(cls := "ui inverted inspector menu", style := "margin-top: 0; border-radius: 0;")(
        div(cls := "right menu")(
          a(cls := "active item", attr("data-tab") := "flag-tab")(i(cls := "flag outline icon"), "Flags"),
          a(cls := "item", attr("data-tab") := "syntax-tab")(i(cls := "file outline icon"), "Syntax")
        )
      ).render)
    )

  val editorTab = new Tab("editor-tab", editorPane, true)
  val inspectorTab = new Tab("inspector-tab", inspectorPane)

  val tabPane = new TabPane(editorTab, inspectorTab)

  val vertSplitPane =
    new VerticalSplitPane(viewerPane, tabPane)

  def onShowCell(c: EditorCell): Unit =
    for { face <- c.face } {
      viewer.complex = Some(face)
    }

  def onEditorSelect(c: EditorCell): Unit =
    for { face <- c.face } {

      c.label match {
        case None => {
          jQuery("#label-input").value("")
        }
        case Some(mk) => {
          jQuery("#label-input").value(mk.lbl)
        }
      }
      
    }

  // Oh!  We're actually doing this every time there's a viewer select,
  // not only in the viewer mode.  Probably should fix this for performance
  // reasons.
  def onViewerSelect(c: ViewerCell): Unit =
    for { cmplx <- viewer.complex ; face <- c.face } {

      faceViewer.complex = Some(face)

      // No link if we selected the principal face ...
      if (cmplx.dim - face.dim > 0) {

        val linkItr = new FlagIterator[(Option[SimpleMarker], FaceAddr)](cmplx.withFaceAddresses, Some(c.faceAddress), true, true)

        //  Hacky.  SCardinal needs a method for extracting a face ....
        val card = FlagExtruder.extrudeFrom(linkItr, (None,ThisDim(Nil)))

        val res = card.cardinalComplex.map({
          case Positive() => None
          case Negative() => None
          case Neutral((o,_)) => o
        })

        linkViewer.complex = res.sourceAt(rootCardinalAddr(card.dim).complexAddress)

      } else linkViewer.complex = None

      // Display a list of flags ...
      val flagItr = new FlagIterator(face.withFaceAddresses)
      // This prints the link flag list ....
      // val flagItr = new FlagIterator(cmplx.withFaceAddresses, Some(c.faceAddress), true, true)
      jQuery(flagList).empty()
      // println("******* Flag List *********")

      for { f <- flagItr } {

        val strFlag : Flag[String] =
          f.map({
            case SrcFacet((f , _) , d) => SrcFacet(f.map(_.lbl).getOrElse(""), d)
            case TgtFacet((f , _)) => TgtFacet(f.map(_.lbl).getOrElse(""))
          })

        // println(flagStr(strFlag))

        val item = a(cls := "item", onclick := { () => onSelectFlag(f) })(flagStr(strFlag)).render
        jQuery(flagList).append(item)

      }

      // Show the syntax ...
      // import syntax.SyntaxExport
      // import syntax.PrettyPrinter

      // SyntaxExport.complexToTerm(face) match {
      //   case Xor.Left(msg) => jQuery(syntaxPre).text(msg)
      //   case Xor.Right(tm) => {

      //     jQuery(syntaxPre).empty

      //     val strItr = PrettyPrinter.prettyPrint(
      //       SyntaxExport.toPrintTree(tm), 80, 2
      //     )

      //     for { str <- strItr } {
      //       jQuery(syntaxPre).append(str)
      //     }

      //   }
      // }

    }

  def onSelectFlag(f: Flag[(Option[SimpleMarker], FaceAddr)]): Unit =
    for { faceCmplx <- faceViewer.complex } {

      // Uh, the use of state here is kinda hacky
      var cmplx = faceCmplx.map(
        _.map(_.copy(sourceDec = Map(), targetDec = None))
      )

      f.foreach({
        case SrcFacet((optMk , fa) , dir) =>
          cmplx.applyAt(fa)({
            case None => Some(SimpleMarker("", sourceDec = Map(dir.dir -> EdgeDecoration("circle", "red", false))))
            case Some(mk) => Some(mk.addSourceDec(dir.dir, EdgeDecoration("circle", "red", false)))
          }).foreach(res => { cmplx = res })
        case TgtFacet((optMk , fa)) => 
          cmplx.applyAt(fa)({
            case None => Some(SimpleMarker("", targetDec = Some(EdgeDecoration("circle", "red"))))
            case Some(mk) => Some(mk.withTargetDec(Some(EdgeDecoration("circle", "red"))))
          }).foreach(res => { cmplx = res })
      })

      faceViewer.complex = Some(cmplx)

    }

  def handleResize: Unit = {

    val uiWidth = jQuery("#editor-div").width.toInt
    val uiHeight = jQuery(".content").first.height.toInt

    vertSplitPane.setWidth(uiWidth)
    vertSplitPane.setHeight(uiHeight)

  }

  def autoLabel(letters: Boolean): Unit =
    for {
      tab <- editor.activeTab
    } {

      var lbl: Int = 0
      val card = tab.editor.cardinal

      Traverse[Suite].map(card)((nst: SCardNst[tab.editor.SimpleNeutralCell]) => {

        for { n <- nst.toList.reverse } {
          n.label = if (letters) {
            Some(SimpleMarker((lbl + 97).toChar.toString))
          } else {
            Some(SimpleMarker(lbl.toString))
          }
          lbl += 1
        }

      })

      tab.editor.renderAll
  }

  def updateLabel: Unit =
    for {
      tab <- editor.activeTab
      root <- tab.editor.selectionRoot
    } {

      import latex.LatexParser
      import fastparse.all._

      val inputVal = jQuery("#label-input").value().toString

      val labelVal =
        LatexParser.Input.parse(
          inputVal
        ) match {
          case Parsed.Success(value, _) => value
          case Parsed.Failure(_, _, _) => inputVal
        }

      root.label = 
        root.label match {
          case None => Some(SimpleMarker(labelVal)) 
          case Some(SimpleMarker(l, s, td, sd)) =>
            Some(SimpleMarker(labelVal, s, td, sd))
        }

      tab.editor.renderAll

    }

  def onPaste: Unit =
    for {
      gallery <- viewer.activeGallery
      root <- gallery.selectionRoot
      cell <- root.face
      tab <- editor.activeTab
      root <- tab.editor.selectionRoot
      face <- root.boxFace
      pc <- face.matchWith(cell)
    } {

      pc.foreach({
        case (c, mk) => c.label = mk
      })

      tab.editor.renderAll

    }

  def onEditFace: Unit =
    for {
      gallery <- viewer.activeGallery
      root <- gallery.selectionRoot
      face <- root.face
    } {
      editor.newTab(SCardinal(face))
    }

  def onRefresh: Unit =
    for {
      gallery <- viewer.activeGallery
    } {

      // Install the debug predicate
      gallery.debugPred = (bx: gallery.SimpleActiveCell) => {
        bx.dim == 3
      }

      gallery.renderAll

      // Reset the debug predicate
      gallery.debugPred = (bx: gallery.SimpleActiveCell) => false

    }

  class SketchEntry(val name: String, val id: String) {

    val previewPane = new SimpleViewer[Option[SimpleMarker]]
    previewPane.scale = 1.0

    val uiElement =
      div(
        div(cls := "ui grey inverted top attached segment")(
          previewPane.uiElement
        ),
        div(cls := "ui grey inverted bottom attached right aligned segment", style := "padding: 3px;")(
          button(cls := "ui mini icon button", onclick := { () => onInspectEntry })(i(cls := "eye icon")),
          button(cls := "ui mini icon button", onclick := { () => onEditEntry })(i(cls := "pencil icon")),
          button(cls := "ui mini icon button", onclick := { () => onDeleteEntry })(i(cls := "trash icon"))
        )
      ).render

    def onInspectEntry: Unit = {
      viewer.complex = previewPane.complex
      // jQuery("#inspector-link").click()
    }

    def onEditEntry: Unit =
      for {
        c <- previewPane.complex
      } {
        editor.newTab(SCardinal(c))
        jQuery("#editor-link").click()
      }

    def onDeleteEntry: Unit = {
      onDeleteSketch(this)
    }

  }

  val loadedSketches: Buffer[SketchEntry] = Buffer()

  def isLoggedIn: Boolean = {
    jQuery("#login-status").attr("data-connected").
      toOption.map(_.toBoolean).getOrElse(false)
  }

  def onOpenSketch(content: Element): Unit = {
    for { isLoadedStr <- jQuery(content).attr("data-loaded").toOption } {
      if (! isLoadedStr.toBoolean) {
        for {
          id <- jQuery(content).attr("data-id").toOption
          nm <- jQuery(content).attr("data-name").toOption
        } {

          import upickle.default._
          import opetopic.net.LoadSketchRequest

          val req = LoadSketchRequest(id)

          dom.ext.Ajax.post(
            url = "/getSketch",
            data = write(req),
            headers = Map(
              ("X-Requested-With" -> "*"),
              ("CSRF-Token" -> "nocheck")
            ),
            withCredentials = true
          ).map(_.responseText).foreach(json => {

            val c : SComplex[Option[SimpleMarker]] =
              complexFromJson[Option[SimpleMarker]](upickle.json.read(json))

            val entry = new SketchEntry(nm, id)

            jQuery(content).append(entry.uiElement)
            jQuery(entry.previewPane.uiElement).height(75)
            jQuery(entry.previewPane.uiElement).width(200)
            entry.previewPane.complex = Some(c)

            jQuery(content).attr("data-loaded", true)
            loadedSketches += entry

          })

        }
      }
    }
  }

  def onDeleteSketch(entry: SketchEntry): Unit = {

      import upickle.default._
      import opetopic.net.DeleteSketchRequest

    (if (isLoggedIn) {

      val req = DeleteSketchRequest(entry.id)

      dom.ext.Ajax.post(
        url = "/deleteSketch",
        data = write(req),
        headers = Map(
          ("X-Requested-With" -> "*"),
          ("CSRF-Token" -> "nocheck")
        ),
        withCredentials = true
      ).map(_.responseText).map({
        case "ok" => true
        case _ => false
      })

    } else Future { true }).foreach(removeUi => {
      if (removeUi) {
        jQuery(entry.uiElement).parent.parent.remove
        loadedSketches -= entry
      }
    })

  }


  def onSaveSketch: Unit =
    for { cmplx <- viewer.complex } {

      import org.scalajs.dom
      import upickle.default._
      import opetopic.net.SaveSketchRequest

      val name =
        jQuery("#sketch-name-input").value.asInstanceOf[String] match {
          case "" => "Untitled"
          case s => s
        }

      (if (isLoggedIn) {

        val req = SaveSketchRequest(name, "", "", complexToJson(cmplx))

        dom.ext.Ajax.post(
          url = "/saveSketch",
          data = write(req),
          headers = Map(
            ("X-Requested-With" -> "*"),
            ("CSRF-Token" -> "nocheck")
          ),
          withCredentials = true
        ).map(req => {
          req.responseText.split(" ").toList match {
            case "ok" :: id :: Nil => Some(id)
            case _ => None
          }
        })

      } else Future { None }).map(idOpt => {

        val id = idOpt getOrElse ""
        val entry = new SketchEntry(name, id)
        
        val uiEl = div(cls := "item")(
          a(cls := "title")(i(cls := "dropdown icon"), name),
          div(cls := "content", attr("data-name") := name,
            attr("data-id") := id,
            attr("data-loaded") := true)(
            entry.uiElement
          )
        ).render
        
        jQuery("#editor-left-sidebar").append(uiEl)
        jQuery(entry.previewPane.uiElement).height(75)
        jQuery(entry.previewPane.uiElement).width(200)
        entry.previewPane.complex = Some(cmplx)

        loadedSketches += entry

      })

    }

  def onDownloadSketch: Unit =
    for { cmplx <- viewer.complex } {

      import upickle.default._
      import opetopic.net._

      val renderData : String = complexToJson(cmplx)
      val sizingMethod : String = write(Percentage(0.05))

      jQuery("#sketch-file").value(jQuery("#download-input").value.asInstanceOf[String])
      jQuery("#render-data").value(renderData)
      jQuery("#sizing-mthd").value(sizingMethod)
      jQuery("#render-request-form").submit()
      
    }

  def main: Unit = {

    jQuery("#editor-div").append(vertSplitPane.uiElement)

    jQuery("#label-input").on("input", () => { updateLabel })
    jQuery(".ui.dropdown.item").dropdown(lit(direction = "upward", action = "hide"))
    // kind of a hack to make the tab resize correctly....
    jQuery(".ui.sidebar.menu .item").tab(lit(onVisible = { () => jQuery(dom.window).trigger("resize") }))
    jQuery(".ui.inspector.menu .item").tab()

    jQuery("#editor-left-sidebar").accordion(
      lit(onOpening = { (content: Element) =>
        onOpenSketch(content) } : js.ThisFunction))

    // Install the resize handler and trigger the event
    // to set defaults ...
    jQuery(dom.window).on("resize", () => { handleResize })
    setTimeout(100){
      jQuery(dom.window).trigger("resize")
      vertSplitPane.initialize
    }

    // Render the editor 
    editor.initialize

  }


}
