/**
  * AddrExplorer.scala - A utility for exploring opetopic addresses
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.addrexplorer

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

object AddrExplorer extends JSApp {

  def main: Unit = {
    println("Started AddrExplorer...")
    showEditor
  }

  var activeEditor : Option[EditorView] = None

  def showEditor: Unit = {

    val ed = new EditorView

    jQuery("#article-content").empty().append(ed.uiElement)
    jQuery("#bottom-menu").empty().append(ed.bottomUi)
    jQuery("#toc-pane").empty()
    jQuery("#base-bar").empty()

    ed.editor.initialize
    activeEditor = Some(ed)
    
  }

  def loadExplorer: Unit =
    for {
      ed <- activeEditor
    } {
      ed.editor.rootFace match {
        case None => println("Nothing selected")
        case Some(c) => {

          println("Exploring selected face.")

          val dim = c.dim

          val addrCmplx : SComplex[AddrMarker] =
            c.mapWithAddr({ case (_, fa) =>
              AddrMarker(Addr(dim - fa.codim, fa.address))
            })

          // Here we start a new explorer instance and show it

          val explorer = new Explorer(addrCmplx)

          jQuery("#article-content").empty().append(explorer.uiElement)
          jQuery("#bottom-menu").empty().append(explorer.bottomUi)
          jQuery("#toc-pane").empty().append(explorer.sideUi)
          jQuery("#base-bar").empty().append(explorer.baseUi)

          explorer.initialize

        }
      }
    }

  class EditorView {

    val editor = new JsStableEditor[SimpleMarker]

    val bottomUi = 
      div(cls := "right menu")(
        div(cls := "item")(
          button(cls := "ui green button", onclick := { () => loadExplorer })("Explore Face")
        )
      ).render

    val uiElement =
      div(
        h3(cls := "ui dividing header")("Editor"),
        editor.uiElement
      ).render

  }

  case class Addr(
    val dim: Int,
    val addr: SAddr
  )

  implicit def addrToSAddr(a: Addr): SAddr = a.addr

  case class AddrMarker(
    val addr : Addr,
    val colorSpec : ColorSpec = DefaultColorSpec
  ) {
    def dim = addr.dim
  }

  implicit object AddrMarkerRenderable extends Renderable[AddrMarker] {
    def render(f: UIFramework)(mk: AddrMarker): f.CellRendering = {
      val trRenderer = new TreeRenderer[f.type](f)
      val be = trRenderer.renderAddr(mk.addr.addr).be
      f.CellRendering(be, mk.colorSpec)
    }
  }

  class Explorer(val cmplx: SComplex[AddrMarker]) {

    val dim = cmplx.dim

    val mainViewer = new JsStableViewer[AddrMarker]
    mainViewer.viewerHeight = 350
    mainViewer.complex = Some(cmplx)

    val selectionViewer = new JsStableViewer[AddrMarker]

    var selectedMarker : Option[AddrMarker] = None
    var selectedPredicate : Option[String] = None

    mainViewer.onSelectAsRoot =
      (cell : mainViewer.CellType) => {

        // Render the top address in the control pane
        val topMarker : AddrMarker = cell.label
        val trRenderer = new TreeRenderer[JsDomFramework.type](JsDomFramework)
        val be = trRenderer.renderAddr(topMarker.addr).be
        val view = viewport(200, 200, be.bounds, be.element)
        view.width = 200
        view.height = 200
        jQuery(currentAddrDiv).empty().append(view.uiElement)
        selectedMarker = Some(topMarker)
        refreshPredicate

        // Now update the face viewer
        cell.face.map(f => selectionViewer.complex = Some(f))

      }

    val transformDropdown = 
      div(cls := "ui blue right labeled dropdown icon button")(
        span(cls := "text")("Transform"),
        i(cls := "dropdown icon"),
        div(cls := "menu")(
          div(cls := "item", attr("data-value") := "face")("Face")
        )
      ).render
    
    val baseUi =
      div(style := "padding: 20px")(
        div(cls := "ui segment", style := "min-height: 200px")(
          h4(cls := "ui dividing header")("Selected Face"),
          selectionViewer.uiElement,
          transformDropdown
        )
      ).render

    val predicateDropdown = 
      div(cls := "ui blue right labeled dropdown icon button")(
        span(cls := "text")("Predicate"),
        i(cls := "dropdown icon"),
        div(cls := "menu")(
          div(cls := "item", attr("data-value") := "dimMatch")("Dimension Match"),
          div(cls := "item", attr("data-value") := "prefix")("Prefix Of"),
          div(cls := "item", attr("data-value") := "lexPred")("Lex Predecessor")
        )
      ).render

    val currentAddrDiv = div().render

    val sideUi =
      div(
        h3(cls := "ui dividing header")("Control Panel"),
        div(cls := "ui segment")(
          h4(cls := "ui dividing header")("Comparison Predicates"),
          currentAddrDiv, 
          predicateDropdown
        )
      ).render

    val bottomUi = 
      div(cls := "right menu")(
        div(cls := "item")(
          button(cls := "ui red button", onclick := { () => showEditor })("Finish")
        )
      ).render

    val uiElement =
      div(
        h3(cls := "ui dividing header")("Address Complex"),
        mainViewer.uiElement
      ).render

    def initialize: Unit = {

      mainViewer.initialize
      selectionViewer.initialize

      jQuery(predicateDropdown).dropdown(lit(
        onChange = (predicateName: String) => {
          selectedPredicate = Some(predicateName)
          refreshPredicate
        }
      ))

      jQuery(transformDropdown).dropdown(lit(
        action = "hide",
        onChange = (predicateName: String) => {
          println("transform changed")
        }
      ))
      
    }

    def refreshPredicate: Unit =
      selectedPredicate match {
        case None => ()
        case Some("dimMatch") => selectedMarker.map(mk => applyPredicate(new DimensionMatchPredicate(mk.dim)))
        case Some("prefix") => selectedMarker.map(mk => applyPredicate(new DimensionPrefixPredicate(mk.addr)))
        case Some("lexPred") => selectedMarker.map(mk => applyPredicate(new LexPredicate(mk.addr)))
        case _ => ()
      }
    
    def applyPredicate(pred: AddrPredicate): Unit = {

      for {
        gallery <- mainViewer.activeGallery
      } {
        val bcmplx = gallery.boxComplex
        bcmplx.foreach(cell => {
          val mk = cell.label
          val cs = if (pred(mk.addr))
            SatisfiesColorSpec
          else
            DefaultColorSpec
          cell.label = mk.copy(colorSpec = cs)
        })
      }

      mainViewer.refreshViewer
      mainViewer.resizeViewer

    }

  }

  //
  // Useful operations on addresses
  //

  implicit class SAddrOps(a: SAddr) {

    def isPrefixOf(b: SAddr): Boolean =
      if (b.length >= a.length) {
        b.drop(b.length - a.length) == a
      } else false

    def isExtensionOf(b: SAddr): Boolean =
      b.isPrefixOf(a)

    // // Yeah, I see why this is weird. It shouldn't be a "forall", rather,
    // // it should check down to the end.
    // def isLexPredecessorOf(d: Int, b: SAddr): Boolean =
    //   if (d == 0) a.isPrefixOf(b) else {

    //     (a zip b).map({
    //       case (SDir(aa), SDir(bb)) => aa.isPrefixOf(bb) // aa.isLexPredecessorOf(d-1, bb)
    //     }).forall(b => b)

    //   }

    def isLexPredecessorOf(b: SAddr): Boolean =
      if (a == Nil) true
      else if (b == Nil) (a == Nil)
      else {
        val SDir(alst) = a.last
        val SDir(blst) = b.last

        if (alst == blst) 
          a.init.isLexPredecessorOf(b.init)
        else alst.isLexPredecessorOf(blst)
        
      }

  }

  implicit def toSAddrOps(a: Addr): SAddrOps =
    new SAddrOps(a.addr)

  //
  //  Various Predicates
  //

  trait AddrPredicate {
    def apply(addr: Addr): Boolean
  }

  class DimensionMatchPredicate(d: Int) extends AddrPredicate {
    def apply(addr: Addr): Boolean = d == addr.dim
  }

  class DimensionPrefixPredicate(a: Addr) extends AddrPredicate {
    def apply(b: Addr): Boolean = {
      (a.dim == b.dim) && b.isPrefixOf(a)
    }
  }

  class LexPredicate(a: Addr) extends AddrPredicate {
    def apply(b: Addr): Boolean = {
      (a.dim == b.dim) && b.isLexPredecessorOf(a)
    }
  }

  object SatisfiesColorSpec extends ColorSpec(
    fill = "red",
    fillHovered = "red",
    fillSelected = "red",
    stroke = "#000000",
    strokeHovered = "#000000",
    strokeSelected = "#000000",
    edgeHovered = "#f19091"
  )

}

