/**
  * LoginPage.scala - The Login Page
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.pages

import org.scalajs.jquery._
import scalatags.JsDom.all._
import opetopic.js.JQuerySemanticUI._

object LoginPage extends Page {

  val url = "/login"

  def render = {

    val usernameInput = input(`type` := "text").render
    val passwordInput = input(`type` := "password").render

    val signupButton = button(
      `type` := "button", 
      cls := "ui green button",
      onclick := { () => println("Going to signup ...") }
    )("Signup!")

    div(cls := "ui container")(
      div(cls := "ui segment")(
        h3(cls := "ui dividing header")("Login"),
        form(cls := "ui form")(
          div(cls := "field")(label("Username"), usernameInput),
          div(cls := "field")(label("Password"), passwordInput),
          signupButton
        )
      )
    ).render

  }

}
