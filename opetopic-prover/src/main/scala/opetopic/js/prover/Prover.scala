/**
  * Prover.scala - Opetopic Theorem Prover
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import scalaz.std.string._

import opetopic._
import opetopic.tt._
import opetopic.js._
import opetopic.net._
import JQuerySemanticUI._

import syntax.complex._
import syntax.tree._
import syntax.nesting._
import syntax.suite._

import upickle.default._

object Prover extends JSApp {

  def main : Unit = {

    println("Launched Opetopic Prover.")

    jQuery("#new-defn-btn").on("click", () => newDefinition)
    jQuery("#new-module-btn").on("click", () => newModule)
    jQuery(".menu .item").tab()
    jQuery(".ui.accordion").accordion()

  }

  //============================================================================================
  // MODULE MANAGEMENT
  //

  var activeModule : Option[Module] = None

  def newModule: Unit = {

      jQuery("#new-module-modal").modal(lit(
        onApprove = () => {

          val name = jQuery("#module-name-input").value.asInstanceOf[String]
          val imports = jQuery("#module-imports-input").value.asInstanceOf[String]
          val description = jQuery("#module-desc-input").value.asInstanceOf[String]

          // Have to parse the imports ...
          val m = new Module(name, imports.split(" ").toList, description)
          m.isLoaded = true
          val mItem = m.uiElement

          jQuery("#module-list").append(mItem)
          jQuery(mItem).dropdown()
          
          editModule(m)

        }
      )).modal("show")

  }

  def editModule(m: Module): Unit = {

    val mItem = m.uiElement

    jQuery("#defns-hdr").text("Definitions - " + m.name)
    m.showEntries
    activeModule = Some(m)

  }

  def saveModule(m: Module): Unit = {

    val req = SaveModuleRequest(
      m.moduleId,
      m.name,
      m.description,
      m.toCode
    )

    dom.ext.Ajax.post(
      url = "/saveModule",
      data = write(req),
      headers = Map(
        ("X-Requested-With" -> "*"),
        ("CSRF-Token" -> "nocheck")
      ),
      withCredentials = true
    ).map(_.responseText).foreach(s =>
      m.moduleId = Some(s)  // Record the returned UUID
    )


  }

  def newDefinition: Unit = 
    for {
      m <- activeModule
    } {

      val defnWksp = new DefinitionWorkspace(m)

      jQuery("#defn-tab").empty().append(defnWksp.mainGrid)
      defnWksp.initUI
      defnWksp.newEditor
      defnWksp.extendContext("X", ECat)

      jQuery("#defn-tab-btn").click()

    }


  //============================================================================================
  // USER FEEDBACK ROUTINES
  //

  def showErrorMessage(str: String) : Unit = {

    val closeIcon = i(cls := "close icon").render

    val msg = div(cls := "ui negative message")(
      closeIcon,
      div(cls := "header")("Error:"),
      p(str)
    ).render

    jQuery(closeIcon).on("click", () => {
      jQuery(msg).transition(lit(
        animation = "fade",
        onComplete = () => {
          jQuery(msg).remove()
        }
      ))
    })

    jQuery("#msg-box").append(msg)

  }

  def showInfoMessage(str: String) : Unit = {

    val closeIcon = i(cls := "close icon").render

    val msg = div(cls := "ui yellow message")(
      closeIcon,
      div(cls := "header")("Info:"),
      p(str)
    ).render

    jQuery(closeIcon).on("click", () => {
      jQuery(msg).transition(lit(
        animation = "fade",
        onComplete = () => {
          jQuery(msg).remove()
        }
      ))
    })

    jQuery("#msg-box").append(msg)

  }

}
