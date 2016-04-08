/**
  * Sketchpad.scala - Opetopic Sketchpad Application
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import scala.scalajs.{js => sjs}
import sjs.Dynamic.{literal => lit}
import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.jquery._
import scalatags.JsDom.all._

import scalajs.concurrent.JSExecutionContext.Implicits.queue

import opetopic._
import opetopic.ui._
import opetopic.js._
import syntax.tree._
import syntax.complex._
import syntax.suite._
import markers._
import JsDomFramework._
import JQuerySemanticUI._
import SimpleMarker._

object Sketchpad extends JSApp {

  val editor = new SketchpadEditor
  val viewer = new SketchpadViewer

  import editor._

  def main : Unit = {

    println("Launched Opetopic Sketchpad.")

    jQuery("#editor-div").append(editor.uiElement)
    editor.initialize

    jQuery("#viewer-div").append(viewer.uiElement)
    viewer.initialize

    jQuery("#fill-color-btn").popup(lit(
      popup = jQuery(".color-select.popup"),
      movePopup = false,
      on = "click",
      onShow = () => { isFill = true }
    ))

    jQuery("#stroke-color-btn").popup(lit(
      popup = jQuery(".color-select.popup"),
      movePopup = false,
      on = "click",
      onShow = () => { isFill = false }
    ))

    jQuery("#label-input").on("input", () => { updateLabel })

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

  }

  //   jQuery("#snapshot-btn").on("click", () => { takeSnapshot })
  //   jQuery("#code-btn").on("click", () => { showScalaCode })
  //   jQuery("#save-btn").on("click", () => { saveSketch })

  //   jQuery("#sketch-list").find(".item").each({ (d: dom.Element) => 
  //     val idOpt = jQuery(d).attr("data-id").toOption

  //     jQuery(d).on("click", () => {
  //       for { id <- idOpt } { loadSketch(id) }
  //     })

  //   })


  // }

  var isFill: Boolean = true
  var fillColor: String = "white"
  var strokeColor: String = "black"

  def unescapeUnicode(str: String): String =
    """\\u([0-9a-fA-F]{4})""".r.replaceAllIn(str,
      m => Integer.parseInt(m.group(1), 16).toChar.toString)

  def updateLabel: Unit = {

    val labelVal = unescapeUnicode(
      jQuery("#label-input").value().toString
    )

    for {
      _ <- withSelection(new BoxAction[Unit] {

        def objectAction(box : EditorBox[_0]) : Unit = {

          box.optLabel match {
            case None => 
              box.optLabel = Some(SimpleObjectMarker(labelVal, DefaultColorSpec))
            case Some(SimpleObjectMarker(l, s)) => 
              box.optLabel = Some(SimpleObjectMarker(labelVal, s))
          }

          box.panel.refresh

        }

        def cellAction[P <: Nat](p : P)(box: EditorBox[S[P]]) : Unit = {

          box.optLabel match {
            case None => 
              box.optLabel = Some(SimpleCellMarker(labelVal, DefaultColorSpec))
            case Some(SimpleCellMarker(l, s, r, e)) => 
              box.optLabel = Some(SimpleCellMarker(labelVal, s, r, e))
          }

          box.panel.refresh
        }

      })
    } { 

      refreshEditor 
      editor.showFace

    }

  }

  def updateFillColor: Unit = {

    val (f, fh, fs) = colorTripleGen(fillColor)

    for {
      _ <- withSelection(new BoxAction[Unit] {

        def objectAction(box : EditorBox[_0]) : Unit = {

          box.optLabel match {
            case None => 
              box.optLabel = Some(SimpleObjectMarker("", DefaultColorSpec.copy(fill = f, fillHovered = fh, fillSelected = fs)))
            case Some(SimpleObjectMarker(l, s)) => 
              box.optLabel = Some(SimpleObjectMarker(l, s.copy(fill = f, fillHovered = fs, fillSelected = fs)))
          }

          box.panel.refresh

        }

        def cellAction[P <: Nat](p : P)(box: EditorBox[S[P]]) : Unit = {

          box.optLabel match {
            case None => box.optLabel = 
              Some(SimpleCellMarker("", DefaultColorSpec.copy(fill = f, fillHovered = fh, fillSelected = fs)))
            case Some(SimpleCellMarker(l, s, r, e)) => 
              box.optLabel = Some(SimpleCellMarker(l, s.copy(fill = f, fillHovered = fh, fillSelected = fs), r, e))
          }

          box.panel.refresh

        }
      })
    } {

      refreshEditor
      editor.showFace

    }

  }


  def updateStrokeColor: Unit = {

    val (st, sh, ss) = colorTripleGen(strokeColor)

    for {
      _ <- withSelection(new BoxAction[Unit] {

        def objectAction(box : EditorBox[_0]) : Unit = {

          box.optLabel match {
            case None => 
              box.optLabel = Some(SimpleObjectMarker("", DefaultColorSpec.copy(stroke = st, strokeHovered = sh, strokeSelected = ss)))
            case Some(SimpleObjectMarker(l, s)) => 
              box.optLabel = Some(SimpleObjectMarker(l, s.copy(stroke = st, strokeHovered = sh, strokeSelected = ss)))
          }

          box.panel.refresh

        }

        def cellAction[P <: Nat](p : P)(box: EditorBox[S[P]]) : Unit = {

          box.optLabel match {
            case None => 
              box.optLabel = Some(SimpleCellMarker("", DefaultColorSpec.copy(stroke = st, strokeHovered = sh, strokeSelected = ss)))
            case Some(SimpleCellMarker(l, s, r, e)) => 
              box.optLabel = Some(SimpleCellMarker(l, s.copy(stroke = st, strokeHovered = sh, strokeSelected = ss), r, e))
          }

          box.panel.refresh

        }
      })
    } {

      refreshEditor
      editor.showFace

    }

  }

  // def takeSnapshot: Unit = 
  //   for {
  //     tab <- activeTab
  //     bs <- tab.activeBox
  //     lc <- bs.value.labelComplex
  //   } {

  //     val exporter = new SvgExporter(lc)

  //     jQuery(".ui.modal.svgexport").find("#exportlink").
  //       attr(lit(href = "data:text/plain;charset=utf-8," ++
  //         sjs.URIUtils.encodeURIComponent(exporter.svgString)))

  //     jQuery(".ui.modal.svgexport").modal("show")

  //   }

  // def showScalaCode: Unit = 
  //   for {
  //     tab <- activeTab
  //     bs <- tab.activeBox
  //     lc <- bs.value.labelComplex
  //   } {

  //     // import opetopic.pprint._

  //     // implicit val c : Config = Config()
  //     // import tab._

  //     // val test = ScalaPPrint.pprintComplex(lc).mkString

  //     // jQuery("#code-text").empty().text(test)
  //     // jQuery(".ui.modal.codeexport").modal("show")

  //     // Sketchpad.editor.getDoc().setValue(pprintComplex(lblCmplx))
  //     // jQuery(".ui.modal").modal("show")
  //     // Sketchpad.editor.refresh()

  //   }

  // def loadSketch(id: String): Unit = 
  //   for {
  //     tab <- activeTab
  //   } {

  //     import upickle.default._
  //     import opetopic.net.LoadSketchRequest

  //     val req = LoadSketchRequest(id)

  //     dom.ext.Ajax.post(
  //       url = "/getSketch",
  //       data = write(req),
  //       headers = Map(
  //         ("X-Requested-With" -> "*"),
  //         ("CSRF-Token" -> "nocheck")
  //       ),
  //       withCredentials = true
  //     ).map(_.responseText).foreach(json => {

  //       val fc : FiniteComplex[OptMarker] =
  //         Complex.fromJson[OptMarker](upickle.json.read(json))

  //       addEditorTab(Some(fc))

  //     })

  //   }

  // def saveSketch: Unit = 
  //   for {
  //     tab <- activeTab
  //     bs <- tab.activeBox
  //     lc <- bs.value.labelComplex
  //   } {

  //     import org.scalajs.dom
  //     import upickle.default._
  //     import opetopic.net.SaveSketchRequest

  //     jQuery("#save-modal").modal(lit(
  //       onApprove = () => {

  //         val req = 
  //           SaveSketchRequest(
  //             jQuery("#sketch-name-input").value.asInstanceOf[String],
  //             jQuery("#sketch-path-input").value.asInstanceOf[String],
  //             jQuery("#sketch-desc-input").value.asInstanceOf[String],
  //             Complex.toJson(lc)
  //           )

  //         dom.ext.Ajax.post(
  //           url = "/saveSketch",
  //           data = write(req),
  //           headers = Map(
  //             ("X-Requested-With" -> "*"),
  //             ("CSRF-Token" -> "nocheck")
  //           ),
  //           withCredentials = true
  //         ).map(_.responseText).foreach(println)

  //       }
  //     )).modal("show")

  //   }

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
      case "grey"   => ("lightgrey", "darkgrey", "grey")
      case "black"  => ("#1B1C1D", "#1B1C1D", "#1B1C1D")
      case _ => ("#FFFFFF", "#F3F4F5", "#DCDDDE")
    }

}

