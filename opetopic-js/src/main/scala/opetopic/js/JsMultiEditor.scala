/**
  * JsMultiEditor.scala - Base javascript multieditor implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.ui._
import JsDomFramework._
import JQuerySemanticUI._

class JsMultiEditor[A: Renderable] {

  val editor = new MultiEditor[A, JsDomFramework.type](JsDomFramework)

  def initialize: Unit = {

    // Install the key handler
    jQuery(uiElement).keypress((e : JQueryEventObject) => {
      e.which match {
        case 101 => doExtrude
        case 100 => doDrop
        case 115 => doSprout
        case _ => ()
      }
    })

    showValueEditor
    refreshLayers

  }

  def refreshLayers: Unit = {

    jQuery(layerMenu).empty()

    if (editor.layers.length > 0) {
      for {
        l <- editor.layers
      } {

        l.viewer.renderAll

        val layerItem = a(cls := "item", onclick := { () => onEditLayer(l) })(
          l.viewer.element.uiElement
        ).render

        jQuery(layerMenu).append(layerItem)

      }
    }

  }

  def onNewLayer: Unit = {
    editor.addLayer
    refreshLayers
  }

  def onEditLayer(l: editor.Layer): Unit = {
    if (editor.state != editor.LayerEdit) {
      editor.editLayer(l)
      showLayerEditor
      jQuery(footer).empty().append(layerFooter)
    }
  }

  def onLayerFinish: Unit = {
    editor.closeLayer
    showValueEditor
  }

  def showValueEditor: Unit = {
    editor.valueEditor.renderAll
    jQuery(editorPane).empty().append(editor.valueEditor.element.uiElement)
  }

  def showLayerEditor: Unit = {
    editor.layerEditor.renderAll
    jQuery(editorPane).empty().append(editor.layerEditor.element.uiElement)
  }

  def doExtrude: Unit = {
    editor.extrude
  }

  def doDrop: Unit = {
  }

  def doSprout: Unit = {
  }

  //============================================================================================
  // UI ELEMENTS
  //

  val editorPane = div(cls := "twelve wide center aligned column").render
  val layerMenu = div(cls := "ui vertical fluid menu").render

  val midPane = div(cls := "ui middle attached nofocus segment", tabindex := 0)(
    div(cls := "ui divided grid", style := "min-height: 500px")(
      editorPane,
      div(cls := "four wide column")(
        div(cls := "ui grid")(
          div(cls := "eight wide column")(h3(cls := "ui header")("Layers")),
          div(cls := "eight wide right aligned column")(
            div(cls := "ui icon button", onclick := { () => onNewLayer })(i(cls := "ui plus icon"))
          )
        ),
        layerMenu
      )
    )
  ).render
  
  val topMenu =
    div(cls := "ui top attached menu")(
      div(cls := "ui dropdown item")(
        "Shape", i(cls := "dropdown icon"),
        div(cls := "vertical fluid menu")(
          div(cls := "item", style := "min-width: 150px", onclick := { () => () })(span(cls := "description")("e"), "Extrude"),
          div(cls := "item", onclick := { () => () })(span(cls := "description")("d"), "Drop"),
          div(cls := "item", onclick := { () => () })(span(cls := "description")("s"), "Sprout")
        )
      )
    ).render

  val valueFooter =
    div(p("Value pane.")).render

  val layerFooter =
    div(
      div(cls := "ui button", onclick := { () => onLayerFinish })("Finish")
    ).render

  val footer =
    div(cls := "ui bottom attached segment")(valueFooter).render

  val uiElement =
    div(topMenu, midPane, footer).render

}
