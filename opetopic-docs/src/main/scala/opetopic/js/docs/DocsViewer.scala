/**
  * DocsViewer.scala - Complex viewer for the documentation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.docs

import opetopic._
import opetopic.js._
import opetopic.ui._
import markers._
import JsDomFramework._
import SimpleMarker._
import Examples._

class DocsViewer(divHeight: Int = 200) extends JsComplexViewer[OptStr](divHeight) {

  // override val config: GalleryConfig = 
  //   GalleryConfig (
  //     panelConfig = PanelConfig(),
  //     width = 600,
  //     height = 350,
  //     spacing = 1500,
  //     minViewX = Some(60000),
  //     minViewY = Some(6000),
  //     spacerBounds = Bounds(0, 0, 600, 600),
  //     manageViewport = true
  //   )

  object TutorialColorSpec extends ColorSpec(
    fill = "#f5f5f5",
    fillHovered = "#f19091",
    fillSelected = "#DCDDDE",
    stroke = "#000000",
    strokeHovered = "#000000",
    strokeSelected = "#000000"
  )

  implicit val vf : VisualizableFamily[OptStr] =
    new VisualizableFamily[OptStr] {
      def visualize[N <: Nat](n: N)(o: OptStr[N]) =
        o match {
          case None => Visualization(n)(TutorialColorSpec, spacer(config.spacerBounds))
          case Some(s) => Visualization(n)(TutorialColorSpec, text(s))
        }
    }

}

