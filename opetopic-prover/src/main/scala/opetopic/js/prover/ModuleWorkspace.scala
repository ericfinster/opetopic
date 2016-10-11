/**
  * ModuleWorkspace.scala - Module workspace
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scala.collection.mutable.ListBuffer

import org.scalajs.jquery._
import org.scalajs.dom.Element
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

trait Workspace {

  def tocPane: Element
  def articlePane: Element

}

class ModuleWorkspace extends Workspace {

  val modules: ListBuffer[Module] = ListBuffer()
  var activeModule : Option[Module] = None

  def createModule(name: String, desc: String): Unit = {

    // Have to parse the imports ...
    val m = new Module(name)
    m.description = desc
    m.isLoaded = true

    val moduleItem = a(cls := "item")(m.name).render

    // val mItem =
    //   div(cls := "ui dropdown item", attr("data-name") := name, attr("data-id") := "")(
    //     i(cls := "dropdown icon"),
    //     name,
    //     div(cls := "menu")(
    //       a(cls := "edit-item item", onclick := { () => editModule(m) })("Edit"),
    //       a(cls := "save-item item", onclick := { () => /* saveModule(m) */ () })("Save"),
    //       a(cls := "delete-item item")("Delete")
    //     )
    //   ).render

    jQuery(moduleList).append(moduleItem)
    modules += m
    activeModule = Some(m)
    // editModule(m)

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

  val articlePane : Element =
    h3(cls := "ui dividing header")("Definitions").render

  def initialize: Unit = {
    // jQuery(tocPane).accordion()
  }

}
