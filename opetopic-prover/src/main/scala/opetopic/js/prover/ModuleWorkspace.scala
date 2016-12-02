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

import opetopic.ott._
import OttPrettyPrinter._
import opetopic.js.JQuerySemanticUI._

trait Workspace {

  def tocPane: Element
  def articlePane: Element

}

class ModuleWorkspace extends Workspace {

  var cm: Option[Editor] = None

  val modules: ListBuffer[Module] = ListBuffer()
  val exports: ListBuffer[Definition] = ListBuffer()

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

  var activeExport: Option[Definition] = None

  def processExports(defns: Seq[Definition]): Unit = {

    exports.clear

    for {
      defn <- defns
    } {
      println("Processing definition: " + defn.id)

      val expItem = a(cls := "item", onclick := { () => { activeExport = Some(defn) }})(defn.id).render
      jQuery(exportsList).append(expItem)

      exports += defn

    }

  }

  def onPasteExport: Unit =
    for {
      defn <- activeExport
      ed <- cm
    } {

      val doc = ed.getDoc()
      val cur = doc.getCursor()
      doc.replaceRange(pprint(defn.declaration), cur, cur)

    }

  //============================================================================================
  // TYPECHECKING OF MODULES
  //

  def typecheckModule: Unit = 
    for {
      m <- activeModule
    } {

      val code: String = m.code

      import java.io.StringReader
      import opetopic.ott.TypeChecker._
      import opetopic.ott.OttSyntax._
      import opetopic.mtl._

      val reader = new StringReader(code)
      val lexer = new OttLexer(reader)
      val parser = new OttParser
      parser.lexer = lexer

      parser.parseAll match {
        case Right(Module(mid, ds)) => {

          jQuery(tcprogress).progress("reset").
            progress("remove success").
            progress(lit(
              total = ds.length
            ))
          
          def checkAll(decls: List[DeclT]): TCM[TCEnv] =
            decls match {
              case Nil => ask
              case d::ds =>
                for {
                  env0 <- checkDecl(d)
                  _ = jQuery(tcprogress).progress("increment")
                  env1 <- withEnv(env0)(checkDecls(ds))
                } yield env1
            }

          // Right, so here we get back the environment.  The idea
          // is that we're going to store it an pass that guy to
          // any new definition.  Then you should be able to paste
          // from that environment into the current definition.

          checkAll(ds).run(TCEnv(Nil, RNil)) match {
            case Xor.Left(msg) => jQuery(tcprogress).progress("set error")
            case Xor.Right(env) => {
              m.isTypechecked = true
              m.moduleEnv = Some(env)
              jQuery(tcprogress).progress("set success")
            }
          }

        }
        case Right(_) => println("Unknown error")
        case Left(s) => println("Parse error: " + s)
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
    textarea(style := "height: auto").render

  val articlePane : Element = div(
    h3(cls := "ui dividing header")("Code View"),
    codeTextArea
  ).render

  val exportsList = div(cls := "ui fluid vertical menu").render

  val tcprogress = div(cls := "ui progress")(
    div(cls := "bar")(div(cls := "progress")),
    div(cls := "label")("Typechecking")
  ).render

  val baseBar =
    div(cls := "ui basic segment", style := "pointer-events: auto")(
      div(cls := "ui secondary pointing menu")(
        a(cls := "active item", attr("data-tab") := "typechecker-tab")("Typechecker"),
        a(cls := "item", attr("data-tab") := "imports-tab")("Imports")
      ),
      div(cls := "ui bottom aligned segment", style := "min-height: 300px;")(
        div(cls := "ui active tab", attr("data-tab") := "typechecker-tab")(
          div(cls := "ui grid")(
            div(cls := "row")(
              div(cls := "two wide center aligned column")(
                button(cls := "ui primary button", onclick := { () => typecheckModule })("Typecheck")
              ),
              div(cls := "fourteen wide column")(
                tcprogress
              )
            )
          )
        ),
        div(cls := "ui tab", attr("data-tab") := "imports-tab")(
          exportsList,
          button(cls := "ui button", onclick := { () => onPasteExport })("Paste to cursor")
        )
      )
    ).render

  val baseMenuItems: Seq[org.scalajs.dom.html.Div] = Seq(
    div(cls := "item")(
      button(cls := "ui primary button", onclick := { () => Prover.newModule })("New Module")
    ).render,
    div(cls := "item")(
      button(cls := "ui primary button", onclick := { () => saveActiveModule })("Save Module")
    ).render,
    div(cls := "item")(
      button(cls := "ui primary button", onclick := { () => deleteActiveModule })("Delete Module")
    ).render,
    div(cls := "right menu")(
      div(cls := "item")(
        button(cls := "ui green button", onclick := { () => Prover.newDefinition })("New Definition")
      )
    ).render
  )

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


    jQuery(tcprogress).progress("reset")

    // Let's setup a codemirror instance.

    val params: EditorConfiguration = EditorConfig.mode("clike").lineNumbers(true).keyMap("emacs") //config
    cm = Some(CodeMirror.fromTextArea(codeTextArea.asInstanceOf[HTMLTextAreaElement], params))

  }

}
