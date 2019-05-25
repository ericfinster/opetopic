/**
  * Editor.scala - Playing with a new editor interface
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor

import scala.scalajs.js
import org.scalajs.jquery._
import js.Dynamic.{literal => lit}

import opetopic._
import opetopic.ui._
import opetopic.js._

import JsDomFramework._
import JQuerySemanticUI._

object Editor {

  val editor = new SimpleCardinalEditor[SimpleMarker]()

  def main: Unit = {

    jQuery("#editor-div").append(editor.uiElement)
    editor.initialize

  }

}
