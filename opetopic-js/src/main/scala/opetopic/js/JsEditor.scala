/**
  * JsEditor.scala - A Cardinal Editor in JavaScript
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs.{js => sjs, _}
import org.scalajs.dom._

import sjs.annotation.JSExport
import sjs.Dynamic.{global => g}

import opetopic._
import opetopic.ui._
import syntax.complex._
import syntax.cardinal._

object JsEditor extends sjs.JSApp {

  def main : Unit = {

    println("Launched Opetopic.")

    // import JsDomFramework._

    // JsDomFramework.onToast = (str: String) => {
    //   val t = g.document.getElementById("msg-toast")
    //   if (t != null) {
    //     t.setAttribute("text", str)
    //     t.show()
    //   }
    // }

    // val baseConfig: GalleryConfig =
    //   GalleryConfig(
    //     panelConfig = defaultPanelConfig,
    //     width = 650,
    //     height = 250,
    //     spacing = 1500,
    //     minViewX = Some(60000),
    //     minViewY = Some(13000),
    //     spacerBounds = Bounds(0, 0, 600, 600)
    //   )

    // val editor = CardinalEditor[ConstString]

    // editor.onBoxClicked = (box: Sigma[editor.NeutralCellBox]) => {

    //   for {
    //     lblCmplx <- box.value.labelComplex
    //   } {

    //     val gallery = ActiveGallery(baseConfig, lblCmplx)(
    //       new AffixableFamily[editor.OptA] {
    //         def apply[N <: Nat](n: N) : Affixable[editor.OptA[N]] = 
    //           Affixable.optionAffixable(baseConfig.spacerBounds, editor.r(n))
    //       }
    //     )

    //     val div = document.getElementById("base-pane")

    //     val lc = div.lastChild
    //     if (lc != null) div.removeChild(lc)

    //     div.appendChild(gallery.element.uiElement)

    //   }

    // }

    // document.onkeydown = ((e: KeyboardEvent) => {
    //   e.keyCode match {
    //     case 69 /* E */ => editor.extrudeSelection
    //     case 76 /* L */ => {
    //       val modal = g.document.getElementById("label-modal")
    //       modal.open()
    //     }
    //     case _ => ()
    //   }
    // })

    // val div = document.getElementById("editor-pane")
    // div.appendChild(editor.element.uiElement)

  }

  // def renderActiveGallery : Unit = {

  //   import JsDomFramework._
  //   import opetopic.syntax.complex._

  //   val gallery = ActiveGallery(baseConfig, fredComplex)

  //   val div = document.getElementById("base-pane")
  //   div.appendChild(gallery.element.uiElement)

  // }

  // def renderActivePanel : Unit = {

  //   import JsDomFramework._

  //   val panel = ActivePanel(fred0)

  //   val panelSvg = document.createElementNS(svgns, "svg")
  //   panelSvg.setAttributeNS(null, "width", "800")
  //   panelSvg.setAttributeNS(null, "height", "600")
  //   panelSvg.setAttributeNS(null, "viewBox", panel.bounds.dimString)

  //   panelSvg.appendChild(panel.element.uiElement)

  //   val div = document.getElementById("editor-pane")
  //   div.appendChild(panelSvg)

  // }

  // def renderStaticPanel : Unit = {

  //   import ScalatagsJsDomFramework._

  //   val panel = StaticPanel(exotic)

  //   val panelSvg = {

  //     import bundle.implicits._
  //     import bundle.svgTags._
  //     import bundle.svgAttrs._

  //     svg(width:="800",height:="600",viewBox:=panel.bounds.dimString,xmlns:="http://www.w3.org/2000/svg")(panel.element)

  //   }

  //   val div = document.getElementById("editor-pane")
  //   div.appendChild(panelSvg.render)

  // }

  // def renderStaticGallery : Unit = {

  //   import ScalatagsJsDomFramework._
  //   import opetopic.syntax.complex._

  //   val gallery = StaticGallery(fredComplex)

  //   val gallerySvg = {

  //     import bundle.implicits._
  //     import bundle.svgTags._
  //     import bundle.svgAttrs._

  //     svg(width:="800",height:="600",viewBox:=gallery.bounds.dimString,xmlns:="http://www.w3.org/2000/svg")(gallery.element)

  //   }

  //   val div = document.getElementById("editor-pane")
  //   div.appendChild(gallerySvg.render)

  // }

}


