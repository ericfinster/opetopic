/**
  * SimpleCardinalEditor.scala - A Simple Cardinal Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor

import scala.collection.mutable.Buffer

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.js._
import opetopic.ui._
import opetopic.mtl._
import JsDomFramework._
import JQuerySemanticUI._

class SimpleCardinalEditor[A: Renderable](c : SCardinal[Option[A]] = SCardinal[A]()) {

  type StableCell = StableEditor[A, JsDomFramework.type]#EditorCell

  var onSelectAsRoot: StableCell => Unit = { _ => () }

  val editor = new StableEditor[A, JsDomFramework.type](JsDomFramework)(c)

  //============================================================================================
  // SHAPE ACTIONS
  //

  def doExtrude: Unit = 
    editor.extrudeSelection

  def doDrop: Unit =
    editor.loopAtSelection

  def doSprout: Unit =
    editor.sproutAtSelection

  // def doExtract: Unit =
  //   for {
  //     tab <- activeTab
  //     ed = tab.editor
  //     pd <- ed.extractSelection
  //     // _ = println("Extracted selection: " + pd.map(_.label).toString)
  //     ccmplx = ed.complex
  //     gpd <- pd.traverse((n: ed.NeutralCell) => {

  //       val addr = n.cardinalAddress.complexAddress
  //       val codim = ccmplx.dim - n.dim
  //       val fa = FaceAddr(codim, addr)

  //       for {
  //         face <- ccmplx.face(fa)
  //       } yield (face : SComplex[ed.EditorCell]).map(_.label)

  //     })
  //     // _ = println("About to graft")
  //     pr <- graft(gpd)({ case (fst, _) => Some(fst) })
  //     (web, srcs) = pr
  //   } {

  //     // println("Finished graft.")

  //     // val newComplex : SComplex[Option[A]] =
  //     //   web >> SBox(None, srcs) >> SDot(None)

  //     newEditor(SCardinal(web, srcs))

  //   }

  // This is a bit of a frankenstein and could be better
  // organized if the extraction routine above returned
  // a decent value ....
  // def doCompose: Unit =
  //   for {
  //     tab <- activeTab
  //     ed = tab.editor
  //     root <- ed.selectionRoot

  //     (cardCmplx: SComplex[ed.EditorCell]) = ed.complex
  //     ccmplx = cardCmplx.map(_.label)
  //     codim = ccmplx.dim - root.dim
  //     ca = root.cardinalAddress.complexAddress
  //     fa = FaceAddr(codim, ca)

  //     pd <- ed.extractSelection
  //     gpd <- pd.traverse((n: ed.NeutralCell) => {

  //       val laddr = n.cardinalAddress.complexAddress
  //       val lfa = FaceAddr(codim, laddr)

  //       ccmplx.face(lfa)

  //     })

  //     pr <- graft(gpd)({ case (fst, _) => Some(fst) })
  //     (web, srcs) = pr

  //     contractor = web >> SBox(None, srcs) >> SDot(None)
  //     compCmplx <- contractAt(ccmplx, contractor, fa)

  //   } {

  //     println("Composition complete")

  //     SCardinal.fromCardinalComplex(compCmplx) match {
  //       case None => println("Error parsing cardinal complex")
  //       case Some(card) => newEditor(card)
  //     }

  //   }

  //============================================================================================
  // ACTIONS
  //

  // def updateLabel(f: Option[A] => Option[A]): Unit = 
  //   for {
  //     tab <- activeTab
  //     root <- tab.editor.selectionRoot
  //   } {
  //     root.label = f(root.label)
  //     refreshEditor
  //     // root.selectAsRoot
  //   }

  // def rootAction(f: StableCell => Unit): Unit = 
  //   for {
  //     tab <- activeTab
  //     root <- tab.editor.selectionRoot
  //   } { f(root) }

  // def withRoot[B](f: StableCell => Option[B]): Option[B] = 
  //   for {
  //     tab <- activeTab
  //     root <- tab.editor.selectionRoot
  //     res <- f(root)
  //   } yield res

  // def rootFace: Option[SComplex[Option[A]]] =
  //   withRoot(_.face)

  // def refreshEditor: Unit = 
  //   for {
  //     tab <- activeTab
  //   } { tab.editor.renderAll }

  //============================================================================================
  // UI ELEMENTS
  //

  val uiElement =
    div(tabindex := 0, style := "padding:20px; min-height:400px")(
      editor.element.uiElement
    ).render

  //============================================================================================
  // UI STATE
  //

  var width : Int = 0
  var height : Int = 0

  //============================================================================================
  // INITIALIZATION
  //

  def initialize: Unit = {

    editor.onCellClick =
      (c: editor.EditorCell) => { }

    editor.onSelectAsRoot =
      (c: StableCell) => onSelectAsRoot(c)

    editor.layoutWidth = bnds => width
    editor.layoutHeight = bnds => height

    def refreshDimensions: Unit = {
      editor.galleryViewport.width = width
      editor.galleryViewport.height = height
    }

    // The idea is that we're going to hook the resize event
    // on the parent element and manage the viewport ourselves..

    width = jQuery(uiElement).width.toInt
    height = jQuery(uiElement).height.toInt

    editor.renderAll
    refreshDimensions

    // Install the key handler
    jQuery(uiElement).keypress(handleKeyEvent(_))

    jQuery(dom.window).on("resize", () => {
      width = jQuery(uiElement).width.toInt
      // height = jQuery(uiElement).height.toInt
      refreshDimensions
    })

  }

  def handleKeyEvent(ev: JQueryEventObject): Unit = {
    ev.which match {
      case 101 => doExtrude
      case 100 => doDrop
      case 115 => doSprout
      // case 120 => doExtract
      // case 99 => doCompose
      case _ => ()
    }
  }

}
