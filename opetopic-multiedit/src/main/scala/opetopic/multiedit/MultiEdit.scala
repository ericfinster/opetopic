/**
  * MultiEdit.scala - A multitopic editor implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.multiedit

import scala.scalajs.{js => sjs}
import sjs.Dynamic.{literal => lit}
import org.scalajs.dom
import org.scalajs.dom._
import org.scalajs.jquery._
// import scalatags.JsDom.all._
import scala.scalajs.js.timers._

// import scalajs.concurrent.JSExecutionContext.Implicits.queue

import opetopic._
import opetopic.ui._
import opetopic.js._
import opetopic.js.ui._
import mtl._

import JsDomFramework._
import JQuerySemanticUI._

object MultiEdit {

  val me = new MultiEditor[SimpleMarker, JsDomFramework.type](JsDomFramework)

  val innerControlPane = new CardinalEditorPane(me.innerControlEditor)
  val outerControlPane = new CardinalEditorPane(me.outerControlEditor)

  val multiEditPane = new CardinalEditorPane(me.dblEditor)

  val controlPanes =
    new HorizontalSplitPane(
      innerControlPane,
      outerControlPane
    )

  val content =
    new VerticalSplitPane(
      multiEditPane,
      controlPanes
    )

  def handleResize: Unit = {

    val uiWidth = jQuery("#editor-div").width.toInt
    val uiHeight = jQuery(".content").first.height.toInt

    content.setWidth(uiWidth)
    content.setHeight(uiHeight)

  }

  def main: Unit = {

    jQuery("#editor-div").append(content.uiElement)

    jQuery(dom.window).on("resize", () => { handleResize })
    setTimeout(100){
      jQuery(dom.window).trigger("resize")

      // Make dividers active
      controlPanes.initialize
      content.initialize

      // Setup event handling in the editors
      innerControlPane.initialize
      outerControlPane.initialize
      multiEditPane.initialize

    }

  }

}

