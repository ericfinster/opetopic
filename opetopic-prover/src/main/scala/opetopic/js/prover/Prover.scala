/**
  * Prover.scala - Opetopic Theorem Prover
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import scalaz.std.string._

import opetopic._
import opetopic.tt._
import opetopic.js._
import opetopic.net._
import OpetopicTypeChecker._
import JQuerySemanticUI._

import syntax.complex._
import syntax.tree._
import syntax.nesting._
import syntax.suite._

import scalajs.concurrent.JSExecutionContext.Implicits.queue

import autowire._

object Prover extends JSApp {

  def main : Unit = {

    println("Launched Opetopic Prover.")

    jQuery("#new-defn-btn").on("click", () => newDefinition)
    jQuery(".menu .item").tab()
    jQuery(".ui.accordion").accordion()

    jQuery("#login-btn").on("click", () => {

      jQuery("#login-modal").modal(lit(
        onApprove = () => {

          val username = jQuery("#username-input").value().asInstanceOf[String]
          val passwd = jQuery("#password-input").value().asInstanceOf[String]

          UserManager.createUser(username, passwd)

        }
      )).modal("show")

    })

  }

  //============================================================================================
  // DEFINITION MANAGEMENT
  //

  var gma: Gamma = Nil
  var rho: Rho = RNil

  def newDefinition: Unit = {

    val defnWksp = new DefinitionWorkspace(rho, gma)

    jQuery("#defn-tab").empty().append(defnWksp.mainGrid)
    defnWksp.initUI
    defnWksp.newEditor
    defnWksp.extendContext("X", ECat)

  }

  def addDefinition(id: String, expr: Expr, exprTy: Expr): EditorM[Unit] = {

    import PrettyPrinter._

    val decl = Def(PVar(id), exprTy, expr)

    for {
      g <- simpleCheck(
        checkD(rho, gma, decl)
      )
    } yield {

      Prover.showInfoMessage("Checked Declaration: " ++ decl.toString)

      val title =
        div(cls := "title")(
          i(cls := "dropdown icon"),
          id
        ).render

      val content =
        div(cls := "content")(
          p(cls := "transition hidden")(
            id ++ " : " ++ prettyPrint(exprTy) ++ " = " ++ prettyPrint(expr)
          )
        ).render

      jQuery("#defn-list").append(title, content)

      // Update the context and environment
      rho = UpDec(rho, decl)
      gma = g

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
