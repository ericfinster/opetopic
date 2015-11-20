/**
  * CodeBlockPane.scala - A Pane containing a code block
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import org.scalajs.jquery._
import scalatags.JsDom.all._

import org.denigma.codemirror.extensions.EditorConfig
import org.denigma.codemirror.{CodeMirror, EditorConfiguration, Editor}
import org.scalajs.dom.raw.HTMLTextAreaElement

import opetopic.tt._
import OpetopicParser._
import OpetopicTypeChecker._

import opetopic.js.JQuerySemanticUI._

class CodeBlockPane {

  var editor : Option[Editor] = None

  def initialize: Unit = {

    val params : EditorConfiguration =
      EditorConfig.lineNumbers(true)

    editor = Some(CodeMirror.fromTextArea(codeArea, params))

  }

  val codeArea = 
    textarea.render

  val paneElement = 
    div(cls := "ui raised attached segment")(codeArea).render

  val bottomElement = 
    div(cls := "ui bottom attached raised right aligned segment")(
      button(cls := "ui primary button", onclick := { () => typecheck })("TypeCheck")
    ).render

  val uiElement = div(tabindex := 0)(
    paneElement,
    bottomElement,
    div()
  ).render

  def typecheck: Unit = 
    for {
      e <- editor
    } {

      println("Going to typecheck ...")

      val lines : String = e.getDoc.getValue()

      parseAll(phrase(expr), lines) match {
        case Success(e, _) => {

          println("Parsing succesful, now typechecking ...")

          import scalaz.-\/
          import scalaz.\/-

          check(RNil, Nil, e, Unt) match {
            case -\/(str) => println("Failure: " ++ str)
            case \/-(()) => println("Success!")
          }

        }
        case err => println(err.toString)
      }
      
    }

}
