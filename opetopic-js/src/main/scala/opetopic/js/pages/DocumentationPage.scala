/**
  * DocumentationPage.scala - The documentation page
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.pages

import scalatags.JsDom.all._

object DocumentationPage extends Page {

  val url = "/docs"

  def render = 
    p("This is the documentation page").render

}
