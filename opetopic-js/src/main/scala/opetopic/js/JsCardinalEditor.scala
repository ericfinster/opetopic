/**
  * JsCardinalEditor.scala - Generic client side CardinalEditor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.collection.mutable.ListBuffer

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import scalaz.\/
import scalaz.\/-
import scalaz.-\/

import scalaz.std.string._

import opetopic._
import syntax.complex._
import JsDomFramework._
import JQuerySemanticUI._

abstract class JsCardinalEditor[A[_ <: Nat]] { thisJsEditor =>

  type OptA[N <: Nat] = Option[A[N]]

  implicit val vf: VisualizableFamily[A]

  val minY : Int = 10000

  //============================================================================================
  // INSTANCE CLASS
  //

  class EditorInstance(co: Option[FiniteComplex[OptA]]) {

    val ce = 
      co match {
        case None => CardinalEditor[A]
        case Some(c) => CardinalEditor[A, c.N](c.value)
      }

    ce.onSelectAsRoot = (boxsig: Sigma[InstanceBox]) => {

      // @natElim
      // def runSelectEvent[N <: Nat](n: N)(box: InstanceBox[N]) : Unit = {
      //   case (Z, box) => onObjectSelect(ce)(box)
      //   case (S(p: P), box) => onCellSelect[P](p)(ce)(box)
      // }

      rootBox = Some(boxsig)
      // runSelectEvent(boxsig.n)(boxsig.value)
      onSelect(boxsig.n)(ce)(boxsig.value)  // This is a duplicate, but I think better.

    }

    ce.onRefresh = () => { 

      val bnds = ce.bounds

      val newBnds = 
        if (minY < bnds.height)
          bnds
        else {
          val offset = minY - bnds.height
          bnds.copy(y = bnds.y - (offset / 2), height = minY)
        }

      ce.galleryViewport.setBounds(newBnds) 

    }

    ce.refreshAll
    refreshDimensions

    type InstanceBox[N <: Nat] = ce.CardinalCellBox[N]
    var rootBox : Option[Sigma[InstanceBox]] = None

    def refreshDimensions: Unit = {

      ce.galleryViewport.width = tabWidth
      ce.galleryViewport.height = tabHeight

    }

    @natElim
    def dispatch[T, N <: Nat](n: N)(b: InstanceBox[N], a: InstanceAction[T]) : T = {
      case (Z, b, a) => a.objectAction(this)(b)
      case (S(p), b, a) => a.cellAction(p)(this)(b)
    }

    def dispatchWithRoot[T](a: InstanceAction[T]) : Option[T] = 
      for {
        boxSig <- rootBox
      } yield dispatch(boxSig.n)(boxSig.value, a)

  }

  //============================================================================================
  // ACTION CLASS
  //

  // This should be the default, I think ....
  trait InstanceAction[T] {
    def objectAction(i: EditorInstance)(b: i.InstanceBox[_0]) : T
    def cellAction[P <: Nat](p: P)(i: EditorInstance)(b: i.InstanceBox[S[P]]) : T
  }

  def actionWithSelection[T](action: InstanceAction[T]) : Option[T] = 
    for {
      i <- activeEditor
      r <- i.dispatchWithRoot(action)
    } yield r

  def selectedFaceComplex: Option[FiniteComplex[OptA]] = 
    for {
      i <- activeEditor
      boxsig <- i.rootBox
      fc <- toOpt(boxsig.value.labelComplex)
    } yield fc

  // Old version, you should remove this from the sketchpad ...
  type EditorBox[N <: Nat] = CardinalEditor[A]#CardinalCellBox[N]

  trait BoxAction[T] {
    def objectAction(box : EditorBox[_0]) : T
    def cellAction[P <: Nat](p : P)(box: EditorBox[S[P]]) : T
  }
    
  @natElim
  def dispatchAction[T, N <: Nat](n: N)(box: EditorBox[N], action: BoxAction[T]) : T = {
    case (Z, box, action) => action.objectAction(box)
    case (S(p), box, action) => action.cellAction(p)(box)
  }

  def withSelection[T](action: BoxAction[T]) : Option[T] =
    for {
      instance <- activeEditor
      boxsig <- instance.rootBox
    } yield dispatchAction(boxsig.n)(boxsig.value, action)

  //============================================================================================
  // SHAPE ACTIONS
  //

  def doExtrude: Unit = 
    for { editor<- activeEditor } {
      editor.ce.extrudeSelection
    }

  def doDrop: Unit = 
    for { editor<- activeEditor } {
      editor.ce.extrudeDrop
    }

  def doPrepend: Unit = 
    for { editor<- activeEditor } {
      editor.ce.sprout
    }

  //============================================================================================
  // SELECTION HANDLERS
  //

  def onSelect[N <: Nat](n: N)(editor: CardinalEditor[A])(box: editor.CardinalCellBox[N]): Unit = ()

  //============================================================================================
  // EDITOR MANAGEMENT
  //

  val instances: ListBuffer[EditorInstance] = ListBuffer()

  var editorCount: Int = 0
  var activeEditor: Option[EditorInstance] = None

  def newEditor(co: Option[FiniteComplex[OptA]] = None) : Unit = {

    val editor = new EditorInstance(co)
    instances += editor
    editorCount += 1

    val cntStr = editorCount.toString
    val tabName = "tab-" ++ cntStr

    val tabItem = a(cls := "item", "data-tab".attr := tabName)(cntStr).render
    val tab = div(cls := "ui tab", "data-tab".attr := tabName)(
      editor.ce.element.uiElement
    ).render

    jQuery(paginationMenu).append(tabItem)
    jQuery(tabPane).append(tab)

    jQuery(tabItem).tab(lit(
      onVisible = (s: String) => { activeEditor = Some(editor) }
    ))

    jQuery(tabItem).click()

  }

  def refreshEditor: Unit = 
    for {
      instance <- activeEditor
    } { instance.ce.refreshGallery }

  //============================================================================================
  // UI STATE
  //

  var tabWidth : Int = 0
  var tabHeight : Int = 0

  //============================================================================================
  // UI ELEMENTS
  //

  val tabPane = div(cls := "ui middle attached nofocus segment", tabindex := 0, style := "min-height: 300px").render
  val paginationMenu = div(cls := "ui pagination menu").render
  
  val topMenu = 
    div(cls := "ui top attached menu")(
      div(cls := "ui dropdown item")(
        "Shape", i(cls := "dropdown icon"),
        div(cls := "vertical fluid menu")(
          div(cls := "item", style := "min-width: 150px", onclick := { () => doExtrude })(span(cls := "description")("e"), "Extrude"),
          div(cls := "item", onclick := { () => doDrop })(span(cls := "description")("d"), "Extrude Drop"),
          div(cls := "item", onclick := { () => doPrepend })(span(cls := "description")("p"), "Prepend Glob")
        )
      )
    ).render

  val bottomMenu = 
    div(cls := "ui bottom attached segment")(
      div(cls := "ui grid")(
        div(cls := "four column row")(
          div(cls := "left floated column")(
            paginationMenu
          ),
          div(cls := "right floated right aligned column")(
            button(cls := "ui icon button", onclick := { () => newEditor() })(i(cls := "add icon"))
          )
        )
      )
    ).render

  val uiElement = 
    div(topMenu, tabPane, bottomMenu).render

  //============================================================================================
  // INITIALIZATION
  //

  def initialize: Unit = {

    // The idea is that we're going to hook the resize event
    // on the parent element and manage the viewport ourselves..

    tabWidth = jQuery(tabPane).width.toInt
    tabHeight = jQuery(tabPane).height.toInt

    // println("Tab dimensions are: (" + tabWidth.toString + ", " + tabHeight.toString + ")")

    // Install the key handler
    jQuery(uiElement).keypress((e : JQueryEventObject) => {
      e.which match {
        case 101 => doExtrude
        case 100 => doDrop
        case 112 => doPrepend
        case _ => ()
      }
    })

    jQuery(topMenu).
      find(".dropdown.item").
      dropdown(lit(action = "hide"))

    jQuery(dom.window).on("resize", () => { resizeInstances })

    newEditor()

  }

  def resizeInstances: Unit = {

    tabWidth = jQuery(tabPane).width.toInt

    for {
      instance <- instances
    } { instance.refreshDimensions }

  }

}
