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

    render

  }

  // I would like to try and generate a polymer style element in javascript and 
  // to load it.  For this I would like to use scalatags

  // I'm not quite sure if this is possible.  It seems to be that the actual component
  // definitions need to be inside html.  But I think it should be possible to still
  // connect up to a scala backend

  // In any case, all of this is kind of silly if you don't start importing the
  // actual components which already exist from google.  And for this you are 
  // going to need to fiddle with a bit of bower setup ....

  @JSExport
  def recieveTap: Unit = {
    println("Got a tap at the backend.");
  }

  //============================================================================================
  // EXPERIMENTS
  //

  import ScalatagsJsDomFramework._
  import opetopic.Examples._

  @JSExport
  def render : Unit = {

    val panel = Panel(exotic)

    val panelSvg = {
      import bundle.implicits._
      import bundle.svgTags._
      import bundle.svgAttrs._

      import opetopic.syntax.nesting._

      val baseBox = panel.boxNesting.baseValue

      val viewboxStr = 
        (baseBox.x - (panel.externalPadding * 4)).toString ++ " " ++ (baseBox.y - (panel.externalPadding * 4)).toString ++ " " ++
          (baseBox.width + (panel.externalPadding * 8)).toString ++ " " ++ (baseBox.height + (panel.externalPadding * 8)).toString

      svg(width:="400",height:="400",viewBox:=viewboxStr,xmlns:="http://www.w3.org/2000/svg")(panel.render)

    }
    

    val div = document.getElementById("my-content")
    div.appendChild(panelSvg.render)

  }


}
