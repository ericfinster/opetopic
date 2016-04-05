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
import scala.concurrent.Future

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

    setupModules

  }

  //============================================================================================
  // MODULE MANAGEMENT
  //

  val modules: ListBuffer[Module] = ListBuffer()
  var activeModule : Option[Module] = None

  def newModule: Unit = {

      jQuery("#new-module-modal").modal(lit(
        onApprove = () => {

          val name = jQuery("#module-name-input").value.asInstanceOf[String]
          val description = jQuery("#module-desc-input").value.asInstanceOf[String]

          // Have to parse the imports ...
          val m = new Module(name)
          m.description = description
          m.isLoaded = true

          val mItem = 
            div(cls := "ui dropdown item", "data-name".attr := name, "data-id".attr := "")(
              i(cls := "dropdown icon"),
              name,
              div(cls := "menu")(
                a(cls := "edit-item item", onclick := { () => editModule(m) })("Edit"),
                a(cls := "save-item item", onclick := { () => saveModule(m) })("Save"),
                a(cls := "delete-item item")("Delete")
              )
            ).render

          jQuery("#module-list").append(mItem)
          jQuery(mItem).dropdown()
          
          modules += m
          editModule(m)

        }
      )).modal("show")

  }

  def editModule(m: Module): Unit = {

    val loadModuleFuture : Future[Unit] = 
      if (m.isLoaded) 
        Future.successful(()) 
      else {

        m.moduleId match {
          case None => Future.failed(new IllegalStateException("No module id"))
          case Some(muuid) => {

            val req = LoadModuleRequest(m.name, muuid)

            dom.ext.Ajax.post(
              url = "/getModule",
              data = write(req),
              headers = Map(
                ("X-Requested-With" -> "*"),
                ("CSRF-Token" -> "nocheck")
              ),
              withCredentials = true
            ).map(xhtml => m.loadData(xhtml.responseText))

          }
        }

      }


    loadModuleFuture onSuccess { 
      case () => {
        jQuery("#defns-hdr").text("Definitions - " + m.name)
        activeModule = Some(m)
        m.showEntries
      }
    }

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


  def setupModules: Unit = {

    jQuery("#module-list").children().each((e: dom.Element) => 
      for {
        mname <- jQuery(e).attr("data-name").toOption 
        muuid <- jQuery(e).attr("data-id").toOption
      } {
        
        val m = new Module(mname)
        m.moduleId = Some(muuid)
        modules += m

        jQuery(e).dropdown()
        jQuery(e).find(".edit-item").on("click", () => editModule(m))
        jQuery(e).find(".save-tiem").on("click", () => saveModule(m))

      }
    )

  }

  //============================================================================================
  // ACTION EXECUTION
  //

  def runAction(act: EditorM[Unit]) : Unit = {

    import scalaz.\/
    import scalaz.\/-
    import scalaz.-\/

    act match {
      case -\/(msg: String) => Prover.showErrorMessage(msg)
      case \/-(_) => ()
    }

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
