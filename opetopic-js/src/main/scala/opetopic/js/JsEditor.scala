/**
  * JsEditor.scala - A Cardinal Editor in JavaScript
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import scala.scalajs.{js => sjs}
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._

import opetopic._
import opetopic.ui._
import syntax.complex._
import syntax.cardinal._
import JsDomFramework._
import JQuerySemanticUI._

object JsEditor extends JSApp {

  val baseConfig: GalleryConfig =
    GalleryConfig(
      panelConfig = defaultPanelConfig,
      width = 650,
      height = 150,
      spacing = 1500,
      minViewX = Some(60000),
      minViewY = Some(13000),
      spacerBounds = Bounds(0, 0, 600, 600)
    )


  def initUI: Unit = {
    jQuery(".main.menu .ui.dropdown").dropdown(
      sjs.Dynamic.literal(on = "hover")
    )
  }

  val editor = CardinalEditor[ConstString]
  editor.onSelectAsRoot = showBoxProperties

  def main : Unit = {

    initUI

    println("Launched Opetopic.")

    jQuery("body").keypress((e : JQueryEventObject) => {
      e.which match {
        case 101 => editor.extrudeSelection
        case 100 => editor.extrudeDrop
        case 112 => editor.sprout
        case 109 => jQuery(".ui.sidebar").sidebar("toggle")
        case _ => ()
      }
    })

    jQuery("#label-input").on("input", (e: JQueryEventObject) => {
      updateLabel
    })

    val div = document.getElementById("editor-pane")
    div.appendChild(editor.element.uiElement)

  }

  var currentBox: Option[Sigma[editor.CardinalCellBox]] = None

  def updateLabel : Unit =
    for {
      boxsig <- currentBox
      box = boxsig.value.asInstanceOf[editor.NeutralCellBox[boxsig.N]]
    } {

      val theText = jQuery("#label-input").value

      if (theText != null) {
        box.optLabel = Some(theText.toString)
      }

      box.panel.refresh
      editor.refreshGallery

    }

  def showBoxProperties(boxsig: Sigma[editor.CardinalCellBox]) : Unit = {

    currentBox = Some(boxsig)
    val box = boxsig.value.asInstanceOf[editor.NeutralCellBox[boxsig.N]]

    box.optLabel match {
      case None => jQuery("#label-input").value("")
      case Some(str) => jQuery("#label-input").value(str)
    }

    for {
      lblCmplx <- box.labelComplex
    } {

      // import JsPrinter._

      // jQuery("#tt-expr").value(prettyPrintComplex(lblCmplx.length.pred)(lblCmplx).mkString("\n\n"))

      val gallery = ActiveGallery(baseConfig, lblCmplx)(
        new AffixableFamily[editor.OptA] {
          def apply[N <: Nat](n: N) : Affixable[editor.OptA[N]] =
            Affixable.optionAffixable(baseConfig.spacerBounds, editor.r(n))
        }
      )

      val div = document.getElementById("face-preview-pane")

      val lc = div.lastChild
      if (lc != null) div.removeChild(lc)

      div.appendChild(gallery.element.uiElement)

    }

  }

}


// JsDomFramework.onToast = (str: String) => {
//   val t = g.document.getElementById("msg-toast")
//   if (t != null) {
//     t.setAttribute("text", str)
//     t.show()
//   }
// }

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


