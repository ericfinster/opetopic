/**
  * JsEditor.scala - A Cardinal Editor in JavaScript
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs._
import org.scalajs.dom._

import js.annotation.JSExport

import scalatags.JsDom.all._

object JsEditor extends js.JSApp {

  def main : Unit = {

    println("Launched Opetopic.")

    renderPanel

  }

  //============================================================================================
  // EXPERIMENTS
  //

  import opetopic.Examples._

  def renderPanel : Unit = {

    import JsDomFramework._

    val panel = ActivePanel(exotic)

    val panelSvg = document.createElementNS(svgns, "svg")
    panelSvg.setAttributeNS(null, "width", "800")
    panelSvg.setAttributeNS(null, "height", "600")
    panelSvg.setAttributeNS(null, "viewBox", panel.bounds.dimString)

    panelSvg.appendChild(panel.element.uiElement)

    val div = document.getElementById("middle-pane")
    div.appendChild(panelSvg)

  }

  def renderGallery : Unit = {

    import ScalatagsJsDomFramework._
    import opetopic.syntax.complex._

    val gallery = StaticGallery(fredComplex)

    val gallerySvg = {

      import bundle.implicits._
      import bundle.svgTags._
      import bundle.svgAttrs._

      svg(width:="800",height:="600",viewBox:=gallery.bounds.dimString,xmlns:="http://www.w3.org/2000/svg")(gallery.element)

    }

    val div = document.getElementById("middle-pane")
    div.appendChild(gallerySvg.render)

  }


}
