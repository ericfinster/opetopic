/**
  * Tutorial.scala - Main Entry for Opetopic Tutorial
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.docs

import scala.scalajs.{js => sjs}
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.ui._
import opetopic.js._
import JQuerySemanticUI._
import JsDomFramework.{defaultGalleryConfig => _, _}
import syntax.complex._

object Docs extends JSApp {

  import Examples._

  implicit val galleryConfig : GalleryConfig =
    GalleryConfig(
      panelConfig = defaultPanelConfig,
      width = 600,
      height = 350,
      spacing = 1500,
      minViewX = Some(60000),
      minViewY = Some(6000),
      spacerBounds = Bounds(0, 0, 600, 600)
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
          case None => Visualization(n)(TutorialColorSpec, spacer(galleryConfig.spacerBounds))
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
        case "opetopes" => runOpetopesPage
        case _ => ()
      }
    }

  }

  def runOpetopesPage: Unit = {

    println("Starting the opetopes page ...")

    val gallery = ActiveGallery(threecell)
    jQuery("#gallery-pane").append(gallery.element.uiElement)

  }
    

  //   val hoveredStroke = "#ff2a2a"
  //   val hoveredFill = "#ff2a2a"

  //   val unhoveredStroke = "#000000"
  //   val unhoveredFill = "#000000"

  //   val hoveredAuxFill = TutorialColorSpec.fillHovered
  //   val unhoveredAuxFill = "#000000"

  //   def lblToClass(str: String) : String = 
  //     str match {
  //       case "\u03b1" => "alpha"
  //       case "\u03b2" => "beta"
  //       case "\u03b3" => "gamma"
  //       case "\u03b4" => "delta"
  //       case "\u03b5" => "epsilon"
  //       case "\u03b6" => "zeta"
  //       case "\u03a6" => "phi"
  //       case _ => str
  //     }

  //   gallery.onSelectAsRoot = (bs: Sigma[gallery.GalleryBoxType]) => { 
  //     for {
  //       lc <- bs.value.labelComplex
  //     } {
  //       val faceGallery = ActiveGallery(lc)
  //       jQuery("#face-pane").empty().append(faceGallery.element.uiElement)
  //     }
  //   }

  //   gallery.onHover = (bs : Sigma[gallery.GalleryBoxType]) => {
  //     for {
  //       lbl <- bs.value.label
  //     } {
  //       jQuery(".stroke-" ++ lblToClass(lbl)).css("stroke", hoveredStroke)
  //       jQuery(".fill-" ++ lblToClass(lbl)).css("fill", hoveredFill)
  //     }
  //   }

  //   gallery.onUnhover = (bs : Sigma[gallery.GalleryBoxType]) => {
  //     for {
  //       lbl <- bs.value.label
  //     } {
  //       jQuery(".stroke-" ++ lblToClass(lbl)).css("stroke", unhoveredStroke)
  //       jQuery(".fill-" ++ lblToClass(lbl)).css("fill", unhoveredFill)
  //     }
  //   }


  // }

}

