/**
  * Sketchpad.scala - Opetopic Sketchpad Application
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import scala.scalajs.{js => sjs}
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.ui._
import opetopic.js._
import syntax.tree._
import syntax.complex._
import syntax.suite._
import JsDomFramework._

// import org.denigma.codemirror.extensions.EditorConfig
// import org.denigma.codemirror.{CodeMirror, EditorConfiguration}
// import org.scalajs.dom.raw.HTMLTextAreaElement

import opetopic.js.JQuerySemanticUI._

object Sketchpad extends JSApp {

  // Setup CodeMirror

  // val params : EditorConfiguration = 
  //   EditorConfig.lineNumbers(true) 

  // val codeArea = 
  //   document.getElementById("code-area").
  //     asInstanceOf[HTMLTextAreaElement]

  // val editor = CodeMirror.fromTextArea(codeArea, params)

  def main : Unit = {

    println("Launched Opetopic Sketchpad.")

    jQuery("#new-tab").click((e : JQueryEventObject) => { addEditorTab })
    jQuery("#sketch-prop-tab-menu .item").tab()
    jQuery("#cell-props-menu .item").tab()

    jQuery("#fill-color-btn").popup(sjs.Dynamic.literal(
      popup = jQuery(".color-select.popup"),
      movePopup = false,
      on = "click",
      onShow = () => { isFill = true }
    ))

    jQuery("#stroke-color-btn").popup(sjs.Dynamic.literal(
      popup = jQuery(".color-select.popup"),
      movePopup = false,
      on = "click",
      onShow = () => { isFill = false }
    ))

    jQuery("#label-input").on("input", () => {
      updateLabel
    })

    jQuery(".color-select.popup button").on("click", (e: JQueryEventObject) => {

      val color= jQuery(e.target).attr("data-color").toString

      if (isFill) {

        jQuery("#fill-color-btn").removeClass(fillColor).addClass(color).popup("hide")
        fillColor = color
        updateFillColor

      } else {
        jQuery("#stroke-color-btn").removeClass(strokeColor).addClass(color).popup("hide")
        strokeColor = color
        updateStrokeColor
      }

    })

    jQuery("#snapshot-btn").on("click", () => { takeSnapshot })

    addEditorTab

  }

  var tabCount: Int = 0

  var isFill: Boolean = true
  var fillColor: String = "white"
  var strokeColor: String = "black"

  def addEditorTab: Unit = {

    val editorTab = new EditorTab

    tabCount += 1
    val tc = tabCount.toString
    val tabName = "tab-" ++ tc

    val tabItem = a(cls := "item", "data-tab".attr := tabName)(tc).render
    val tab = div(cls := "ui tab", "data-tab".attr := tabName)(
      editorTab.uiElement
    ).render

    jQuery(".right.menu").before(tabItem)
    jQuery("#sketch-tabs").append(tab)

    jQuery(tabItem).tab(sjs.Dynamic.literal(
      onVisible = (s: String) => { activeTab = Some(editorTab) }
    ))

    jQuery(tabItem).click()

  }

  def unescapeUnicode(str: String): String =
    """\\u([0-9a-fA-F]{4})""".r.replaceAllIn(str,
      m => Integer.parseInt(m.group(1), 16).toChar.toString)

  def updateLabel: Unit = 
    for {
      tab <- activeTab
      bs <- tab.activeBox
    } {

      val labelVal = unescapeUnicode(
        jQuery("#label-input").value().toString
      )

      @natElim
      def doUpdate[N <: Nat](n: N)(box: tab.editor.CardinalCellBox[N]) : Unit = {
        case (Z, box) => 
          box.optLabel match {
            case None => box.optLabel = Some(ObjectMarker(labelVal, DefaultColorSpec))
            case Some(ObjectMarker(l, s)) => box.optLabel = Some(ObjectMarker(labelVal, s))
          }
        case (S(p: P), box) => 
          box.optLabel match {
            case None => box.optLabel = Some(CellMarker(labelVal, DefaultColorSpec))
            case Some(CellMarker(l, s, r, e)) => box.optLabel = Some(CellMarker(labelVal, s, r, e))
          }
      }

      doUpdate(bs.n)(bs.value)

      bs.value.panel.refresh
      tab.editor.refreshGallery
      refreshFaceGallery

    }

  def updateFillColor: Unit = 
    for {
      tab <- activeTab
      bs <- tab.activeBox
    } {

      val (f, fh, fs) = colorTripleGen(fillColor)

      @natElim
      def doUpdate[N <: Nat](n: N)(box: tab.editor.CardinalCellBox[N]) : Unit = {
        case (Z, box) =>
          box.optLabel match {
            case None => box.optLabel = Some(ObjectMarker("", DefaultColorSpec.copy(fill = f, fillHovered = fh, fillSelected = fs)))
            case Some(ObjectMarker(l, s)) => box.optLabel = Some(ObjectMarker(l, s.copy(fill = f, fillHovered = fs, fillSelected = fs)))
          }
        case (S(p: P), box) => 
          box.optLabel match {
            case None => box.optLabel = Some(CellMarker("", DefaultColorSpec.copy(fill = f, fillHovered = fh, fillSelected = fs)))
            case Some(CellMarker(l, s, r, e)) => box.optLabel = Some(CellMarker(l, s.copy(fill = f, fillHovered = fh, fillSelected = fs), r, e))
          }
      }

      doUpdate(bs.n)(bs.value)

      bs.value.panel.refresh
      tab.editor.refreshGallery
      refreshFaceGallery

    }

  def updateStrokeColor: Unit = 
    for {
      tab <- activeTab
      bs <- tab.activeBox
    } {

      val (st, sh, ss) = colorTripleGen(strokeColor)

      @natElim
      def doUpdate[N <: Nat](n: N)(box: tab.editor.CardinalCellBox[N]) : Unit = {
        case (Z, box) =>
          box.optLabel match {
            case None => box.optLabel = Some(ObjectMarker("", DefaultColorSpec.copy(stroke = st, strokeHovered = sh, strokeSelected = ss)))
            case Some(ObjectMarker(l, s)) => box.optLabel = Some(ObjectMarker(l, s.copy(stroke = st, strokeHovered = sh, strokeSelected = ss)))
          }
        case (S(p: P), box) => 
          box.optLabel match {
            case None => box.optLabel = Some(CellMarker("", DefaultColorSpec.copy(stroke = st, strokeHovered = sh, strokeSelected = ss)))
            case Some(CellMarker(l, s, r, e)) => box.optLabel = Some(CellMarker(l, s.copy(stroke = st, strokeHovered = sh, strokeSelected = ss), r, e))
          }
      }

      doUpdate(bs.n)(bs.value)

      bs.value.panel.refresh
      tab.editor.refreshGallery
      refreshFaceGallery

    }

  val propConfig: GalleryConfig =
    GalleryConfig(
      panelConfig = defaultPanelConfig,
      width = 900,
      height = 150,
      spacing = 1500,
      minViewX = Some(60000),
      minViewY = Some(6000),
      spacerBounds = Bounds(0, 0, 600, 600)
    )

  var activeTab: Option[EditorTab] = None

  def refreshFaceGallery: Unit = 
    for {
      tab <- activeTab
      bs <- tab.activeBox
      lc <- bs.value.labelComplex
    } {

      import tab._
      implicit val bsDim = bs.n

      val gallery = ActiveGallery(propConfig, lc)
      jQuery("#face-pane").empty().append(gallery.element.uiElement)

    }

  def displayCell: Unit = 
    for {
      tab <- activeTab
      bs <- tab.activeBox
    } {

      // Okay, I think I see the problem.

      import tab._
      implicit val bsDim = bs.n

      val lblStr = (bs.value.optLabel map (_.label)) getOrElse ""
      jQuery("#label-input").value(lblStr)

      @natElim
      def doRefresh[N <: Nat](n: N)(box: tab.editor.CardinalCellBox[N]) : Unit = {
        case (Z, box) => {
          jQuery("#edge-prop-tab").empty().text("Cell is an object")
          refreshFaceGallery
        }
        case (S(p: P), box) => {
          for {
            lc <- box.labelComplex
          } {

            val frameNesting = lc.tail.head
            val panel = ActivePanel(frameNesting)

            def defaultLeafDecs : Tree[TriangleDec, P] = 
              frameNesting match {
                case Box(bv, cn) => cn map (_ => Nonexistant)
                case _ => throw new IllegalArgumentException("Malformed complex")
              }

            panel.onBoxClicked = { (b: SimpleActiveCellBox[editor.OptA[P], P]) => {

              val curMk : CellMarker[P] = 
                box.optLabel match {
                  case None => CellMarker("", DefaultColorSpec, Nonexistant, Some(defaultLeafDecs))
                  case Some(CellMarker(l, s, r, None)) => CellMarker(l, s, r, Some(defaultLeafDecs))
                  case Some(mk @ CellMarker(l, s, r, Some(_))) => mk
                }

              if (b.isExternal) {

                for {
                  lds <- fromOpt(curMk.leafEdgeDecorations)
                  zp <- lds.seekTo(b.address.head)
                  d <- zp._1.rootValue
                } {

                  val newLds = Zipper.close(p)(zp._2, zp._1.withRootValue(d.next))
                  box.optLabel = Some(curMk.copy(leafEdgeDecorations = Some(newLds)))

                  box.panel.refresh
                  tab.editor.refreshGallery
                  refreshFaceGallery

                }

              } else {

                box.optLabel = Some(curMk.copy(rootEdgeDecoration = curMk.rootEdgeDecoration.next))

                box.panel.refresh
                tab.editor.refreshGallery
                refreshFaceGallery

              }

            }}

            jQuery("#edge-prop-tab").empty().append(panel.element.uiElement)
            refreshFaceGallery

          }
        }
      }

      doRefresh(bs.n)(bs.value)

    }

  def takeSnapshot: Unit = 
    for {
      tab <- activeTab
      bs <- tab.activeBox
      lc <- bs.value.labelComplex
    } {

      val exporter = new SvgExporter(lc)

      jQuery(".ui.modal.svgexport").find("#exportlink").
        attr(sjs.Dynamic.literal(href = "data:text/plain;charset=utf-8," ++
          sjs.URIUtils.encodeURIComponent(exporter.svgString)))

      jQuery(".ui.modal.svgexport").modal("show")

    }

  def colorTripleGen(color: String) : (String, String, String) = 
    color match {
      case "red"    => ("#DB2828", "#DB2828", "#DB2828")
      case "orange" => ("#F2711C", "#F2711C", "#F2711C")
      case "yellow" => ("#FBBD08", "#FBBD08", "#FBBD08")
      case "olive"  => ("#B5CC18", "#B5CC18", "#B5CC18")
      case "green"  => ("#21BA45", "#21BA45", "#21BA45")
      case "teal"   => ("#00B5AD", "#00B5AD", "#00B5AD")
      case "blue"   => ("#2185D0", "#2185D0", "#2185D0")
      case "violet" => ("#6435C9", "#6435C9", "#6435C9")
      case "purple" => ("#A333C8", "#A333C8", "#A333C8")
      case "pink"   => ("#E03997", "#E03997", "#E03997")
      case "brown"  => ("#A5673F", "#A5673F", "#A5673F")
      case "grey"   => ("#767676", "#767676", "#767676")
      case "black"  => ("#1B1C1D", "#1B1C1D", "#1B1C1D")
      case _ => ("#FFFFFF", "#F3F4F5", "#DCDDDE")
    }

}
