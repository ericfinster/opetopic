/**
  * MainPage.scala - The main home page
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.pages

import scalatags.JsDom.all._

object MainPage extends Page {

  val url = "/"

  def render = 
    p("This is the main page").render

}
