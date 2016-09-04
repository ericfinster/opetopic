/**
  * ColorEdit.scala - A colored opetope editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.coloredit

import scala.scalajs.{js => sjs}
import sjs.Dynamic.{literal => lit}
import scala.scalajs.js.JSApp
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.jquery._
import scalatags.JsDom.all._

import scalajs.concurrent.JSExecutionContext.Implicits.queue

import opetopic._
import opetopic.ui._
import opetopic.js._
import mtl._

import JsDomFramework._
import JQuerySemanticUI._

object ColorEdit extends JSApp {

  val editor = new JsColorEditor

  def main: Unit = {

    println("Started ColorEdit...")
    jQuery("#editor-div").append(editor.uiElement)
    editor.initialize

  }

}

