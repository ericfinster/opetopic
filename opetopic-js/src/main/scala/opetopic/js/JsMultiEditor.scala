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

  //============================================================================================
  // UI ELEMENTS
  //

  val midPane = div(cls := "ui middle attached nofocus segment", tabindex := 0, style := "min-height: 500px").render
  
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

  val bottomMenu =
    div(cls := "ui bottom attached segment").render

  val uiElement =
    div(topMenu, midPane, bottomMenu).render


}
