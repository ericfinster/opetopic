/**
  * Prover.scala - Opetopic Theorem Prover
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

import scala.scalajs.js
import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.js._
import opetopic.net._
import opetopic.mtl._
import opetopic.ott._
import OttSyntax._

import JQuerySemanticUI._

import upickle.default._

object Prover extends JSApp {

  val moduleWksp: ModuleWorkspace = new ModuleWorkspace

  def main : Unit = {

    println("Launched Opetopic Prover.")

    showModuleWorkspace(moduleWksp)

    moduleWksp.initialize
    
    jQuery("#new-defn-btn").on("click", () => newDefinition)
    jQuery("#new-module-btn").on("click", () => newModule)

    setupModules

  }

  def showModuleWorkspace(wksp: ModuleWorkspace): Unit = {
    jQuery("#toc-pane").empty().append(wksp.tocPane)
    jQuery("#article-content").empty().append(wksp.articlePane)
    jQuery("#base-bar").empty().append(wksp.baseBar)
  }

  def showDefinitionWorkspace(wksp: DefinitionWorkspace): Unit = {
    jQuery("#toc-pane").empty().append(wksp.tocPane)
    jQuery("#article-content").empty().append(wksp.articlePane)
    jQuery("#base-bar").empty().append(wksp.baseBar)
  }

  //============================================================================================
  // MODULE MANAGEMENT
  //

  def newModule: Unit = {

      jQuery("#new-module-modal").modal(lit(
        onApprove = () => {

          val name = jQuery("#module-name-input").value.asInstanceOf[String]
          val description = jQuery("#module-desc-input").value.asInstanceOf[String]

          moduleWksp.createModule(name, description)

        }
      )).modal("show")

  }

  def loadModule(m: Module): Future[Unit] = {
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
          ).map(xhtml => { m.code = xhtml.responseText })

        }
      }

    }
  }

  def saveModule(m: Module): Unit = {

    val req = SaveModuleRequest(
      m.moduleId,
      m.name,
      m.description,
      m.code
    )

    dom.ext.Ajax.post(
      url = "/saveModule",
      data = write(req),
      headers = Map(
        ("X-Requested-With" -> "*"),
        ("CSRF-Token" -> "nocheck")
      ),
      withCredentials = true
    ).map(_.responseText).foreach(s => {
      println("Response: " + s)
      m.moduleId = Some(s)  // Record the returned UUID
    })

  }

  def deleteModule(m: Module): Unit = {

    m.moduleId match {
      case None => println("No module id")
      case Some(mid) => {

        val req = DeleteModuleRequest(mid)

        dom.ext.Ajax.post(
          url = "/deleteModule",
          data = write(req),
          headers = Map(
            ("X-Requested-With" -> "*"),
            ("CSRF-Token" -> "nocheck")
          ),
          withCredentials = true
        ).map(_.responseText).foreach(s => {
          println("Response: " + s)
        })

      }
    }

  }

  def setupModules: Unit = {

    jQuery("#module-list").children().each((e: dom.Element) => 
      for {
        mname <- jQuery(e).attr("data-name").toOption 
        muuid <- jQuery(e).attr("data-id").toOption
      } {

        println("Processing user module: " + mname)
        moduleWksp.registerUserModule(mname, muuid)

      }
    )

  }

  def newDefinition: Unit =
    for {
      m <- moduleWksp.activeModule
    } {

      val defnWksp = new DefinitionWorkspace(m)
      showDefinitionWorkspace(defnWksp)
      defnWksp.initUI

    }

  //============================================================================================
  // RUN EXCEPT
  //

  def runExcept[A](e: Except[A]): Unit =
    e match {
      case Xor.Left(msg) => Prover.showErrorMessage(msg)
      case Xor.Right(a) => ()
    }

  //============================================================================================
  // TYPECHECKING OF USER CODE
  //

  // def typecheckCode: Unit = {

  //   println("in typecheck routine")

  //   for {
  //     ed <- cm
  //   } {

  //     println("got the editor")

  //     val code : String =
  //       ed.getDoc.getValue()

  //     import java.io.StringReader
  //     import opetopic.ott.TypeChecker._
  //     import opetopic.ott.OttPrettyPrinter._

  //     val reader = new StringReader(code)
  //     val lexer = new OttLexer(reader)
  //     val parser = new OttParser
  //     parser.lexer = lexer

  //     parser.parseAll match {
  //       case Right(Module(mid, ds)) => {

  //         println("Checking module: " + mid)

  //         checkDecls(ds).run(TCEnv(Nil, RNil)) match {
  //           case Xor.Left(msg) => println("Typechecking error: " + msg)
  //           case Xor.Right(_) => println("Success!")
  //         }

  //       }
  //       case Right(_) => println("Unknown error")
  //       case Left(s) => println("Parse error: " + s)
  //     }

  //   }

  // }

  def parseExpr(exprStr: String) : Except[ExpT] = {

    println("In parse routine with: " + exprStr)

    import java.io.StringReader
    val reader = new StringReader(exprStr)
    val lexer = new OttLexer(reader)
    val parser = new OttParser

    try {

      parser.lexer = lexer
      val e: ExpT = parser.parse_Exp1()
      println("Parsed expression: " + e.toString)
      Xor.Right(e)

    } catch {
      case parser.YYError(s) => {
        println("There was an error: " + s)
        Xor.Left(s)
      }
      case e : Error => {
        val s = e.getMessage
        println("There was an error: " + s)
        Xor.Left(s)
      }
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

    println(str)

  }

  def showInfoMessage(str: String) : Unit = {

    val closeIcon = i(cls := "close icon").render

    val lines = str.split("\n").map(p(_).render)

    val msg = div(cls := "ui yellow message")(
      closeIcon,
      div(cls := "header")("Info:")
    ).render

    jQuery(msg).append(lines.toSeq : _*)

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
