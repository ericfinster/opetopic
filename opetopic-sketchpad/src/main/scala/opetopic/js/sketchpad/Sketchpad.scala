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
import JsDomFramework._
import JQuerySemanticUI._

object Sketchpad extends JSApp {

  // val editor = new SketchpadEditor
  // val viewer = new SketchpadViewer

  // import editor._

  def main : Unit = {

    println("Launched Opetopic Sketchpad.")


    // jQuery("#editor-div").append(editor.uiElement)
    // editor.initialize

    // jQuery("#viewer-div").append(viewer.uiElement)
    // viewer.initialize

    // jQuery("#fill-color-btn").popup(lit(
    //   popup = jQuery(".color-select.popup"),
    //   movePopup = false,
    //   on = "click",
    //   onShow = () => { isFill = true }
    // ))

    // jQuery("#stroke-color-btn").popup(lit(
    //   popup = jQuery(".color-select.popup"),
    //   movePopup = false,
    //   on = "click",
    //   onShow = () => { isFill = false }
    // ))

    // jQuery("#label-input").on("input", () => { updateLabel })

    // jQuery(".color-select.popup button").on("click", (e: JQueryEventObject) => {

    //   val color= jQuery(e.target).attr("data-color").toString

    //   if (isFill) {
    //     showFill(color)
    //     updateFillColor
    //   } else {
    //     showStroke(color)
    //     updateStrokeColor
    //   }

    // })

    // jQuery("#edge-accordion").accordion()

    // jQuery("#saved-sketches .item").each((e: dom.Element) => {
    //   for {
    //     suuid <- jQuery(e).attr("data-id").toOption
    //   } {

    //     jQuery(e).on("click", () => {

    //       for { (_, old) <- selectedSketch } { 
    //         jQuery(old).removeClass("active") 
    //       }

    //       jQuery(e).addClass("active")
    //       selectedSketch = Some(suuid, e)

    //     })
        
    //   }
    // })

    // jQuery("#view-btn").on("click", () => { loadSelectedSketch })
    // jQuery("#save-btn").on("click", () => { saveSketch })
    // jQuery("#svg-btn").on("click", () => { renderSketch })
    // jQuery("#export-btn").on("click", () => { exportSketch })

  }

  //   jQuery("#snapshot-btn").on("click", () => { takeSnapshot })
  //   jQuery("#code-btn").on("click", () => { showScalaCode })

  // var isFill: Boolean = true
  // var fillColor: String = "white"
  // var strokeColor: String = "black"
  // var selectedSketch: Option[(String, dom.Element)] = None

  // def unescapeUnicode(str: String): String =
  //   """\\u([0-9a-fA-F]{4})""".r.replaceAllIn(str,
  //     m => Integer.parseInt(m.group(1), 16).toChar.toString)

  // def updateLabel: Unit = {

  //   val labelVal = unescapeUnicode(
  //     jQuery("#label-input").value().toString
  //   )

  //   for {
  //     _ <- withSelection(new BoxAction[Unit] {

  //       def objectAction(box : EditorBox[_0]) : Unit = {

  //         box.optLabel match {
  //           case None => 
  //             box.optLabel = Some(SimpleObjectMarker(labelVal, DefaultColorSpec))
  //           case Some(SimpleObjectMarker(l, s)) => 
  //             box.optLabel = Some(SimpleObjectMarker(labelVal, s))
  //         }

  //         box.panel.refresh

  //       }

  //       def cellAction[P <: Nat](p : P)(box: EditorBox[S[P]]) : Unit = {

  //         box.optLabel match {
  //           case None => 
  //             box.optLabel = Some(SimpleCellMarker(labelVal, DefaultColorSpec))
  //           case Some(SimpleCellMarker(l, s, r, e)) => 
  //             box.optLabel = Some(SimpleCellMarker(labelVal, s, r, e))
  //         }

  //         box.panel.refresh
  //       }

  //     })
  //   } { 

  //     refreshEditor 
  //     editor.showFace

  //   }

  // }

  // def updateFillColor: Unit = {

  //   val (f, fh, fs) = colorTripleGen(fillColor)

  //   for {
  //     _ <- withSelection(new BoxAction[Unit] {

  //       def objectAction(box : EditorBox[_0]) : Unit = {

  //         box.optLabel match {
  //           case None => 
  //             box.optLabel = Some(SimpleObjectMarker("", DefaultColorSpec.copy(fill = f, fillHovered = fh, fillSelected = fs)))
  //           case Some(SimpleObjectMarker(l, s)) => 
  //             box.optLabel = Some(SimpleObjectMarker(l, s.copy(fill = f, fillHovered = fs, fillSelected = fs)))
  //         }

  //         box.panel.refresh

  //       }

  //       def cellAction[P <: Nat](p : P)(box: EditorBox[S[P]]) : Unit = {

  //         box.optLabel match {
  //           case None => box.optLabel = 
  //             Some(SimpleCellMarker("", DefaultColorSpec.copy(fill = f, fillHovered = fh, fillSelected = fs)))
  //           case Some(SimpleCellMarker(l, s, r, e)) => 
  //             box.optLabel = Some(SimpleCellMarker(l, s.copy(fill = f, fillHovered = fh, fillSelected = fs), r, e))
  //         }

  //         box.panel.refresh

  //       }
  //     })
  //   } {

  //     refreshEditor
  //     editor.showFace

  //   }

  // }


  // def updateStrokeColor: Unit = {

  //   val (st, sh, ss) = colorTripleGen(strokeColor)

  //   for {
  //     _ <- withSelection(new BoxAction[Unit] {

  //       def objectAction(box : EditorBox[_0]) : Unit = {

  //         box.optLabel match {
  //           case None => 
  //             box.optLabel = Some(SimpleObjectMarker("", DefaultColorSpec.copy(stroke = st, strokeHovered = sh, strokeSelected = ss)))
  //           case Some(SimpleObjectMarker(l, s)) => 
  //             box.optLabel = Some(SimpleObjectMarker(l, s.copy(stroke = st, strokeHovered = sh, strokeSelected = ss)))
  //         }

  //         box.panel.refresh

  //       }

  //       def cellAction[P <: Nat](p : P)(box: EditorBox[S[P]]) : Unit = {

  //         box.optLabel match {
  //           case None => 
  //             box.optLabel = Some(SimpleCellMarker("", DefaultColorSpec.copy(stroke = st, strokeHovered = sh, strokeSelected = ss)))
  //           case Some(SimpleCellMarker(l, s, r, e)) => 
  //             box.optLabel = Some(SimpleCellMarker(l, s.copy(stroke = st, strokeHovered = sh, strokeSelected = ss), r, e))
  //         }

  //         box.panel.refresh

  //       }
  //     })
  //   } {

  //     refreshEditor
  //     editor.showFace

  //   }

  // }

  // def exportSketch: Unit = 
  //   for {
  //     c <- viewer.complex
  //   } {

  //     val tc = c.value.map(new IndexedMap[OptMarker, ConstString] {
  //       def apply[N <: Nat](n: N)(om: OptMarker[N]) : String = om.toString
  //     })

  //     import opetopic.stable._
  //     val sc = SComplex(tc)

  //     println(sc.toString)

  //   }

  // def renderSketch: Unit = 
  //   for {
  //     c <- viewer.complex
  //   } {

  //     import upickle.default._
  //     import opetopic.net._

  //     val renderData : String = Complex.toJson(c.value)
  //     val sizingMethod : String = write(Percentage(0.05))

  //     jQuery("#render-data").value(renderData)
  //     jQuery("#sizing-mthd").value(sizingMethod)
  //     jQuery("#render-request-form").submit()

  //   }

  // def loadSelectedSketch: Unit = 
  //   for { (id, _) <- selectedSketch } {

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

  //       editor.newEditor(Some(fc))

  //     })
  //   }

  // def saveSketch: Unit = 
  //   for {
  //     c <- viewer.complex
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
  //             Complex.toJson(c.value)
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

  // def colorTripleGen(color: String) : (String, String, String) = 
  //   color match {
  //     case "red"    => ("#DB2828", "#DB2828", "#DB2828")
  //     case "orange" => ("#F2711C", "#F2711C", "#F2711C")
  //     case "yellow" => ("#FBBD08", "#FBBD08", "#FBBD08")
  //     case "olive"  => ("#B5CC18", "#B5CC18", "#B5CC18")
  //     case "green"  => ("#21BA45", "#21BA45", "#21BA45")
  //     case "teal"   => ("#00B5AD", "#00B5AD", "#00B5AD")
  //     case "blue"   => ("#2185D0", "#2185D0", "#2185D0")
  //     case "violet" => ("#6435C9", "#6435C9", "#6435C9")
  //     case "purple" => ("#A333C8", "#A333C8", "#A333C8")
  //     case "pink"   => ("#E03997", "#E03997", "#E03997")
  //     case "brown"  => ("#A5673F", "#A5673F", "#A5673F")
  //     case "grey"   => ("lightgrey", "darkgrey", "grey")
  //     case "black"  => ("#1B1C1D", "#1B1C1D", "#1B1C1D")
  //     case _ => ("#FFFFFF", "#F3F4F5", "#DCDDDE")
  //   }

  // def colorReverseLookup(str: String) : String = 
  //   str match {
  //     case "#DB2828" => "red"
  //     case "#F2711C" => "orange"
  //     case "#FBBD08" => "yellow"
  //     case "#B5CC18" => "olive"
  //     case "#21BA45" => "green"
  //     case "#00B5AD" => "teal"
  //     case "#2185D0" => "blue"
  //     case "#6435C9" => "violet"
  //     case "#A333C8" => "purple"
  //     case "#E03997" => "pink"
  //     case "#A5673F" => "brown"
  //     case "lightgrey" => "grey"
  //     case "#1B1C1D" => "black"
  //     case "#000000" => "black"
  //     case _ => "white"
  //   }

  // def showProps[N <: Nat](m: Option[SimpleMarker[N]]): Unit = 
  //   m match {
  //     case None => {
  //       showFill("white")
  //       showStroke("black")
  //       jQuery("#label-input").value("")
  //     }
  //     case Some(mk) => {
  //       showFill(colorReverseLookup(mk.colorSpec.fill))
  //       showStroke(colorReverseLookup(mk.colorSpec.stroke))
  //       jQuery("#label-input").value(mk.label)
  //     }
  //   }
  // def showFill(str: String): Unit = {
  //   jQuery("#fill-color-btn").removeClass(fillColor).addClass(str).popup("hide")
  //   fillColor = str
  // }

  // def showStroke(str: String): Unit = {
  //   jQuery("#stroke-color-btn").removeClass(strokeColor).addClass(str).popup("hide")
  //   strokeColor = str
  // }

}

