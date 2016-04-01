/**
  * SketchpadPage.scala - The Sketchpad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.pages

import scalatags.JsDom.all._

object SketchpadPage extends Page {

  val url = "/sketchpad"

  def render = 
    p("This is the sketchpad page").render

}
