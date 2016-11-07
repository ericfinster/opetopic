/**
  * ModuleWorkspace.scala - Module workspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.collection.mutable.ListBuffer

import org.scalajs.jquery._
import org.scalajs.dom.Element
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import org.denigma.codemirror.extensions.EditorConfig
import org.denigma.codemirror.{CodeMirror, EditorConfiguration, Editor}
import org.scalajs.dom.raw.HTMLTextAreaElement

trait Workspace {

  def tocPane: Element
  def articlePane: Element

}

class ModuleWorkspace extends Workspace {

  var cm: Option[Editor] = None

  val modules: ListBuffer[Module] = ListBuffer()
  var activeModule : Option[Module] = None

  def createModule(name: String, desc: String): Unit = {

    // Have to parse the imports ...
    val m = new Module(name)
    m.description = desc
    m.isLoaded = true

    val moduleItem = a(cls := "item", onclick := { () => displayModule(m) })(m.name).render

    jQuery(moduleList).append(moduleItem)
    modules += m
    displayModule(m)

  }

  def registerUserModule(name: String, uuid: String): Unit = {

    val m = new Module(name)
    m.moduleId = Some(uuid)

    val moduleItem = a(cls := "item", onclick := { () => displayModule(m) })(m.name).render

    jQuery(moduleList).append(moduleItem)
    modules += m

  }

  def displayModule(m: Module): Unit = {

    println("About to display module: " + m.name)

    Prover.loadModule(m) onSuccess {
      case () => {

        for { ed <- cm } {
          ed.getDoc().setValue(m.code)
        }

        println("Successfully loaded module ...")
        activeModule = Some(m)

      }
    }

  }

  //============================================================================================
  // UI ELEMENTS
  //

  val moduleList: Element = div().render

  val tocPane : Element =
    div(cls := "opetopic ui vertical fluid auxillary menu")(
      div(cls := "header item")("Modules"),
      moduleList
    ).render

  val codeTextArea : Element =
    textarea(style := "min-height: 800px").render

  val articlePane : Element = div(
    h3(cls := "ui dividing header")("Code View"),
    codeTextArea
  ).render

  val baseBar =
    div(cls := "ui basic segment", style := "pointer-events: auto")(
      div(cls := "ui secondary pointing menu")(
        a(cls := "active item")("Actions"),
        a(cls := "item")("Messages")
      ),
      div(cls := "ui segment", style := "min-height: 300px;")(
        button(cls := "ui primary button", onclick := { () => saveActiveModule })("Save Module"),
        button(cls := "ui primary button", onclick := { () => deleteActiveModule })("Delete Module")
      )
    ).render

  def saveActiveModule: Unit =
    for {
      m <- activeModule
      ed <- cm
    } {

      val code = ed.getDoc().getValue()
      m.code = code

      println("About to save module: " + m.name)
      Prover.saveModule(m)

    }

  def deleteActiveModule: Unit = 
    for {
      m <- activeModule
    } {

      println("About to delete: " + m.name)
      Prover.deleteModule(m)

    }

  def initialize: Unit = {

    // jQuery(tocPane).accordion()

    // Let's setup a codemirror instance.

    val params: EditorConfiguration = EditorConfig.mode("clike").lineNumbers(true).keyMap("emacs") //config
    cm = Some(CodeMirror.fromTextArea(codeTextArea.asInstanceOf[HTMLTextAreaElement], params))

    // jQuery("#code-tab-btn").tab(lit(
    //   onFirstLoad = () => {
    //   }
    // ))

    // jQuery("#tc-btn").on("click", () => typecheckCode)

  }

}
