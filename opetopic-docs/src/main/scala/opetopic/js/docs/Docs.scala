/**
  * Tutorial.scala - Main Entry for Opetopic Tutorial
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.docs

import scala.scalajs.{js => sjs}
import sjs.Dynamic.{literal => lit}
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.ui._
import opetopic.js._
import JQuerySemanticUI._
import JsDomFramework._
import syntax.complex._

object Docs extends JSApp {

  import Examples._

  object DocsGalleryConfig extends GalleryConfig (
    panelConfig = PanelConfig(),
    width = 600,
    height = 350,
    spacing = 1500,
    minViewX = Some(60000),
    minViewY = Some(6000),
    spacerBounds = Bounds(0, 0, 600, 600),
    manageViewport = true
  )

  object TutorialColorSpec extends ColorSpec(
    fill = "#f5f5f5",
    fillHovered = "#f19091",
    fillSelected = "#DCDDDE",
    stroke = "#000000",
    strokeHovered = "#000000",
    strokeSelected = "#000000"
  )

  implicit def optStrFamily : VisualizableFamily[OptStr] =
    new VisualizableFamily[OptStr] {
      def visualize[N <: Nat](n: N)(o: OptStr[N]) =
        o match {
          case None => Visualization(n)(TutorialColorSpec, spacer(DocsGalleryConfig.spacerBounds))
          case Some(s) => Visualization(n)(TutorialColorSpec, text(s))
        }
    }

  def main: Unit = {

    println("Started Opetopic Interactive Documentation ...")

    jQuery("#nav-accordion").accordion()

    for {
      pageName <- jQuery("meta[name=page]").attr("content").toOption
    } {
      pageName match {
        case "basicediting" => runBasicEditingPage
        case "diagrams/complexes" => doComplexes
        case "diagrams/opetopes" => doOpetopes
        case "diagrams/geometry" => doGeometry
        case _ => ()
      }
    }

  }

  def doOpetopes: Unit = {

    println("Starting the opetopes page ...")

    val viewer = new DocsViewer(350)
    jQuery("#gallery-pane").append(viewer.uiElement)
    viewer.initialize

    viewer.complex = Some(threecell)

    val faceViewer = new DocsViewer
    jQuery("#face-pane").append(faceViewer.uiElement)
    faceViewer.initialize

    viewer.activeGallery map (g => {
      g.onSelectAsRoot = (bs: Sigma[g.GalleryBoxType]) => {
        for { lc <- bs.value.labelComplex } { faceViewer.complex = Some(lc) }
      }
    })

  }
    
  def runBasicEditingPage: Unit = {

    val editor = new DocsEditor

    jQuery("#editor-div").append(editor.uiElement)
    editor.initialize

  }

  def doComplexes: Unit = {

    jQuery(".ui.checkbox").checkbox(lit(

      onChecked = () => {
        println("checked")
        jQuery("#atoms").fadeTo("slow", 0.0)
        jQuery("#bond").fadeTo("slow", 1.0)
      },

      onUnchecked = () => {
        println("unchecked")
        jQuery("#atoms").fadeTo("slow", 1.0)
        jQuery("#bond").fadeTo("slow", 0.0)
      }

    ))

    jQuery("#cbtn").on("click", () => {
      jQuery("#cbtn").addClass("active")
      jQuery("#fbtn").removeClass("active")
      jQuery("#sbtn").removeClass("active")
      jQuery("#csvg").fadeTo("slow", 1.0)
      jQuery("#fsvg").fadeTo("slow", 0.0)
      jQuery("#ssvg").fadeTo("slow", 0.0)
    })

    jQuery("#fbtn").on("click", () => {
      jQuery("#cbtn").removeClass("active")
      jQuery("#fbtn").addClass("active")
      jQuery("#sbtn").removeClass("active")
      jQuery("#csvg").fadeTo("slow", 0.0)
      jQuery("#fsvg").fadeTo("slow", 1.0)
      jQuery("#ssvg").fadeTo("slow", 0.0)
    })

    jQuery("#sbtn").on("click", () => {
      jQuery("#cbtn").removeClass("active")
      jQuery("#fbtn").removeClass("active")
      jQuery("#sbtn").addClass("active")
      jQuery("#csvg").fadeTo("slow", 0.0)
      jQuery("#fsvg").fadeTo("slow", 0.0)
      jQuery("#ssvg").fadeTo("slow", 1.0)
    })

  }

  def doGeometry: Unit = {

    //============================================================================================
    // HELPERS
    //

    val hoveredStroke = "#ff2a2a"
    val hoveredFill = "#ff2a2a"

    val unhoveredStroke = "#000000"
    val unhoveredFill = "#000000"

    val hoveredAuxFill = TutorialColorSpec.fillHovered
    val unhoveredAuxFill = "#000000"

    def lblToClass(str: String) : String =
      str match {
        case "\u03b1" => "alpha"
        case "\u03b2" => "beta"
        case "\u03b3" => "gamma"
        case "\u03b4" => "delta"
        case "\u03b5" => "epsilon"
        case "\u03b6" => "zeta"
        case "\u03a6" => "phi"
        case _ => str
      }

    def installHandlers(viewer: DocsViewer, el: SnapElement) : Unit = 
      for {
        gallery <- viewer.activeGallery
      } {
        gallery.onHover = (bs : Sigma[gallery.GalleryBoxType]) => {
          for {
            lbl <- bs.value.label
          } {
            el.selectAll(".stroke-" + lblToClass(lbl)).attr(lit(stroke = hoveredStroke))
            el.selectAll(".fill-" + lblToClass(lbl)).attr(lit(fill = hoveredFill))
          }
        }

        gallery.onUnhover = (bs : Sigma[gallery.GalleryBoxType]) => {
          for {
            lbl <- bs.value.label
          } {
            el.selectAll(".stroke-" + lblToClass(lbl)).attr(lit(stroke = unhoveredStroke))
            el.selectAll(".fill-" + lblToClass(lbl)).attr(lit(fill = unhoveredFill))
          }
        }
      }

    //============================================================================================
    // THE OBJECT
    //

    val objectViewer = new DocsViewer(150)
    jQuery("#object-pane").append(objectViewer.uiElement)
    objectViewer.initialize
    objectViewer.complex = Some(obj)

    val objectEl = Snap("#object-svg")
    Snap.load("/assets/svgs/object.svg", (f: Fragment) => {
      objectEl.append(f)
    })

    installHandlers(objectViewer, objectEl)

    //============================================================================================
    // THE ARROW
    //

    val arrowViewer = new DocsViewer(150)
    jQuery("#arrow-pane").append(arrowViewer.uiElement)
    arrowViewer.initialize
    arrowViewer.complex = Some(arrow)

    val arrowEl = Snap("#arrow-svg")
    Snap.load("/assets/svgs/arrow.svg", (f: Fragment) => {
      arrowEl.append(f)
    })

    installHandlers(arrowViewer, arrowEl)

    //============================================================================================
    // THE DROP
    //

    val dropViewer = new DocsViewer(150)
    jQuery("#drop-pane").append(dropViewer.uiElement)
    dropViewer.initialize
    dropViewer.complex = Some(drop)

    val dropEl = Snap("#drop-svg")
    Snap.load("/assets/svgs/drop.svg", (f: Fragment) => {
      dropEl.append(f)
    })

    installHandlers(dropViewer, dropEl)

    //============================================================================================
    // THE TWOGLOB
    //

    val twoglobViewer = new DocsViewer(150)
    jQuery("#twoglob-pane").append(twoglobViewer.uiElement)
    twoglobViewer.initialize
    twoglobViewer.complex = Some(twoglob)

    val twoglobEl = Snap("#twoglob-svg")
    Snap.load("/assets/svgs/twoglob.svg", (f: Fragment) => {
      twoglobEl.append(f)
    })

    installHandlers(twoglobViewer, twoglobEl)

    //============================================================================================
    // THE SIMPLEX
    //

    val simplexViewer = new DocsViewer(150)
    jQuery("#simplex-pane").append(simplexViewer.uiElement)
    simplexViewer.initialize
    simplexViewer.complex = Some(simplex)

    val simplexEl = Snap("#simplex-svg")
    Snap.load("/assets/svgs/simplex.svg", (f: Fragment) => {
      simplexEl.append(f)
    })

    installHandlers(simplexViewer, simplexEl)

    //============================================================================================
    // THE QUAD
    //

    val quadViewer = new DocsViewer(150)
    jQuery("#quad-pane").append(quadViewer.uiElement)
    quadViewer.initialize
    quadViewer.complex = Some(quad)

    val quadEl = Snap("#quad-svg")
    Snap.load("/assets/svgs/quad.svg", (f: Fragment) => {
      quadEl.append(f)
    })

    installHandlers(quadViewer, quadEl)

    //============================================================================================
    // THE THREECELL
    //

    val threecellViewer = new DocsViewer(350)
    jQuery("#threecell-pane").append(threecellViewer.uiElement)
    threecellViewer.initialize
    threecellViewer.complex = Some(threecell)

    val threecellEl = Snap("#threecell-svg")
    Snap.load("/assets/svgs/threecell.svg", (f: Fragment) => {
      threecellEl.append(f)
    })

    installHandlers(threecellViewer, threecellEl)


  }

}

