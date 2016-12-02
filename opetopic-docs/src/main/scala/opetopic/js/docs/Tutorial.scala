/**
  * Tutorial.scala - Main Entry for Opetopic Tutorial
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.docs

import scala.scalajs.js
import org.scalajs.jquery._

import opetopic._
import opetopic.ui._
import opetopic.js._
import JsDomFramework._

object Tutorial {

  import Examples._

  type DocsViewer = JsStableViewer[String]
  type DocsCell = DocsViewer#CellType

  object TutorialRenderable extends Renderable[String] {
    def render(f: UIFramework)(s: String): f.CellRendering = 
      f.CellRendering(f.text(s), TutorialColorSpec)
  }

  object TutorialColorSpec extends ColorSpec(
    fill = "#f5f5f5",
    fillHovered = "#f19091",
    fillSelected = "#DCDDDE",
    stroke = "#000000",
    strokeHovered = "#000000",
    strokeSelected = "#000000",
    edgeHovered = "#f19091"
  )

  def newViewer: DocsViewer = 
    new JsStableViewer[String]()(TutorialRenderable)

  val bondViewer = newViewer
  jQuery("#bond-diagram").append(bondViewer.uiElement)
  bondViewer.viewerHeight = 320
  bondViewer.initialize
  bondViewer.firstPanel = Some(3)
  bondViewer.lastPanel = Some(4)

  val viewer = newViewer
  jQuery("#diag-one").append(viewer.uiElement)
  viewer.viewerHeight = 320
  viewer.initialize

  def start = {

    // Start the reveal.js presentation viewer
    Reveal.initialize()

    val slideChangedHandler : RevealEvent => js.Any =
      (ev: RevealEvent) => {
      }

    Reveal.addEventListener(
      "slidechanged",
      slideChangedHandler,
      false
    )

    Reveal.addEventListener(
      "bond-diagram-slide",
      bondDiagramHandler,
      false
    )

    Reveal.addEventListener(
      "diag-one-slide",
      diagramOneHandler,
      false
    )

    Reveal.addEventListener(
      "fragmentshown",
      fragmentShownHandler,
      false
    )

    Reveal.addEventListener(
      "fragmenthidden",
      fragmentHiddenHandler,
      false
    )
    
  }

  val bondDiagramHandler: RevealEvent => js.Any =
    (ev: RevealEvent) => {
      bondViewer.complex = Some(big)
    }

  val diagramOneHandler : RevealEvent => js.Any =
    (ev: RevealEvent) => {
      //println("Entering diagram one")
      viewer.complex = Some(big)
    }

  val fragmentShownHandler : RevealEvent => js.Any =
    (ev: RevealEvent) => {
      jQuery(ev.fragment).attr("id").toOption match {
        case Some("bond-fragment") => {
          jQuery("#atoms").fadeTo("slow", 0.0)
          jQuery("#bond").fadeTo("slow", 1.0)
        }
        case Some("lower-bond-fragment") => {
          jQuery("#csvg").fadeTo("slow", 0.0)
          jQuery("#fsvg").fadeTo("slow", 1.0)
          jQuery("#ssvg").fadeTo("slow", 0.0)
        }
        case Some("upper-bond-fragment") => {
          jQuery("#csvg").fadeTo("slow", 0.0)
          jQuery("#fsvg").fadeTo("slow", 0.0)
          jQuery("#ssvg").fadeTo("slow", 1.0)
        }
        case _ => println("unknown fragment")
      }

    }

  val fragmentHiddenHandler : RevealEvent => js.Any =
    (ev: RevealEvent) => {
      jQuery(ev.fragment).attr("id").toOption match {
        case Some("bond-fragment") => {
          jQuery("#atoms").fadeTo("slow", 1.0)
          jQuery("#bond").fadeTo("slow", 0.0)
        }
        case Some("lower-bond-fragment") => {
          jQuery("#csvg").fadeTo("slow", 1.0)
          jQuery("#fsvg").fadeTo("slow", 0.0)
          jQuery("#ssvg").fadeTo("slow", 0.0)
        }
        case Some("upper-bond-fragment") => {
          jQuery("#csvg").fadeTo("slow", 0.0)
          jQuery("#fsvg").fadeTo("slow", 1.0)
          jQuery("#ssvg").fadeTo("slow", 0.0)
        }
        case _ => println("unknown fragment")
      }
    }

}
