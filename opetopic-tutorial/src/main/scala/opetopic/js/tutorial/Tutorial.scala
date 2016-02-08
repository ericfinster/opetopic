/**
  * Tutorial.scala - Main Entry for Opetopic Tutorial
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.tutorial

import scala.scalajs.{js => sjs}
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.ui._
import opetopic.js.JsDomFramework.{defaultGalleryConfig => _, _}
import syntax.complex._

object Tutorial extends JSApp {

  def main: Unit = {

    println("Tutorial started ...")

    import Examples._

    implicit val galleryConfig : GalleryConfig =
      GalleryConfig(
        panelConfig = defaultPanelConfig,
        width = 900,
        height = 150,
        spacing = 1500,
        minViewX = Some(60000),
        minViewY = Some(6000),
        spacerBounds = Bounds(0, 0, 600, 600)
      )

    implicit def optStrFamily : VisualizableFamily[OptStr] = 
      new VisualizableFamily[OptStr] {
        def visualize[N <: Nat](n: N)(o: OptStr[N]) = 
          o match {
            case None => Visualization(n)(DefaultColorSpec, spacer(galleryConfig.spacerBounds))
            case Some(s) => Visualization(n)(DefaultColorSpec, text(s))
          }
      }

    val gallery = ActiveGallery(simplex)
    jQuery("#demo").append(gallery.element.uiElement)

  }

}

