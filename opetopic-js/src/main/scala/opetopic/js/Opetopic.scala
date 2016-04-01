/**
  * Opetopic.scala - Main site entry and setup
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs.js.JSApp

import org.scalajs.jquery._
import scalatags.JsDom.all._
import JQuerySemanticUI._

import opetopic.js.pages._

object Opetopic extends JSApp {

  //============================================================================================
  // GLOBAL UI
  //

  val toolsDropdown = 
    div(cls := "ui dropdown item")("Tools",
      i(cls := "dropdown icon"),
      div(cls := "menu")(
        a(cls := "item", onclick := { () => Router.showPage(SketchpadPage) })("SketchPad"),
        a(cls := "item", onclick := { () => Router.showPage(ProverPage) })("Prover")
      )
    ).render

  val topMenu = 
    div(cls := "ui fixed inverted menu")(
      div(cls := "header item")("Opetopic"),
      a(cls := "item", onclick := { () => Router.showPage(MainPage) })("Home"),
      toolsDropdown,
      a(cls := "item", onclick := { () => Router.showPage(DocumentationPage) })("Documentation"),
      div(cls := "right menu")(
        a(cls := "item", onclick := { () => Router.showPage(LoginPage) })("Log In")
      )
    ).render

  val contentDiv = div(cls := "content").render

  //============================================================================================
  // MAIN ENTRY POINT
  //

  def main: Unit = {

    println("Launched opetopic ...")

    // jQuery("body").
    //   append(topMenu).
    //   append(contentDiv)

    jQuery(".ui.dropdown").dropdown()

    // Router.initialize
    // Router.showPage(MainPage)

  }

}


