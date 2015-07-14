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

    renderGallery

  }

  //============================================================================================
  // EXPERIMENTS
  //

  import ScalatagsJsDomFramework._
  import opetopic.Examples._

  def renderPanel : Unit = {

    val panel = StaticPanel(exotic)

    val panelSvg = {

      import bundle.implicits._
      import bundle.svgTags._
      import bundle.svgAttrs._

      svg(width:="800",height:="600",viewBox:=panel.bounds.dimString,xmlns:="http://www.w3.org/2000/svg")(panel.element)

    }
    
    val div = document.getElementById("my-content")
    div.appendChild(panelSvg.render)

  }

  def renderGallery : Unit = {

    import opetopic.syntax.complex._

    val gallery = StaticGallery(fredComplex)

    val gallerySvg = {

      import bundle.implicits._
      import bundle.svgTags._
      import bundle.svgAttrs._

      svg(width:="800",height:="600",viewBox:=gallery.bounds.dimString,xmlns:="http://www.w3.org/2000/svg")(gallery.element)

    }

    val div = document.getElementById("my-content")
    div.appendChild(gallerySvg.render)

  }


}
