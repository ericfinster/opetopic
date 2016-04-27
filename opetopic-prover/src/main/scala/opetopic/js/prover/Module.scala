/**
  * Module.scala - A class holding a module
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.prover

import scala.collection.mutable.ListBuffer

import org.scalajs.dom.Element
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic.tt._
import opetopic.pprint.Tokenizer._
import OTTTypeChecker._
import PrettyPrinter._

class Module(val name: String) { thisModule =>

  sealed trait ModuleEntry { 

    def title: Element
    def content: Element

    def code: String

  }

  case class Declaration(val id: String, val d: Decl) extends ModuleEntry {

    def title =
      div(cls := "title")(
        i(cls := "dropdown icon"),
        id
      ).render

    def content =
      div(cls := "content")(
        p(cls := "transition hidden")(d.pprint)
      ).render

    def code = d.pprint

  }

  var isLoaded: Boolean = false
  var moduleId: Option[String] = None
  var description: String = ""

  val entries: ListBuffer[ModuleEntry] = ListBuffer()

  // The current state of the context
  // and environment
  var gma: Gamma = Nil
  var rho: Rho = RNil

  def addDefinition(id: String, expr: Expr, exprTy: Expr): EditorM[Unit] = {

    val decl : Decl = 
      Def(PVar(id), exprTy, expr)

    for {
      g <- simpleCheck(
        checkD(rho, gma, decl)
      )
    } yield {

      Prover.showInfoMessage("Checked Declaration: " ++ decl.pprint)

      val entry = Declaration(id, decl)
      entries += entry
      showEntry(entry)

      // Update the context and environment
      rho = UpDec(rho, decl)
      gma = g

    }

  }

  def showEntry(me: ModuleEntry): Unit = 
    jQuery("#defn-list").append(me.title, me.content)

  def showEntries: Unit = {
    jQuery("#defn-list").empty().append(
      (entries map (e => List(e.title, e.content))).flatten : _*
    )
  }

  def toCode: String = 
    (entries map (_.code)).mkString("\n\n")

  def writeData: String = {
    import upickle.default._
    write(entries.toList)
  }


  def loadData(data: String): Unit = {

    import upickle.default._

    val ents = read[List[ModuleEntry]](data)

    ents.foreach({
      case (decl @ Declaration(id, d @ Def(p, e, f))) => {
        gma = (id, eval(e, rho)) :: gma
        rho = UpDec(rho, d)
        entries += decl
      }
      case _ => ()
    })

    isLoaded = true

  }

}
