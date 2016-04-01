/**
  * ProverPage.scala - The Prover Page
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.pages

import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic.js.prover.Prover
import opetopic.js.JQuerySemanticUI._

object ProverPage extends Page {

  val url = "/prover"

  def render = {

    val moduleTab =
      div(cls := "ui active tab", "data-tab".attr := "module-tab")(
        h3(cls := "ui dividing header")("Global Environment"),
        div(cls := "ui fluid styled accordion", id := "defn-list")
      ).render

    val definitionTab =
      div(cls := "ui tab", "data-tab".attr := "definition-tab", id := "defn-tab").render

    val content =
      div(
        
        moduleTab,
        definitionTab,

        h3(cls := "ui dividing header")("Messages"),
        div(id := "msg-box"),

        div(cls := "ui inverted bottom fixed menu")(
          a(cls := "active item", "data-tab".attr := "module-tab")("Module"),
          a(cls := "item", "data-tab".attr := "definition-tab")("Definition"),
          div(cls := "item")(
            button(`type` := "button", cls := "ui button",
              onclick := { () => Prover.newDefinition }
            )("New Definition")
          )
        )

      ).render

    content

  }

  override def onShow : Unit = {
    jQuery(".menu .item").tab()
    jQuery(".ui.accordion").accordion()
  }

}
