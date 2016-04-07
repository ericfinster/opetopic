/**
  * EditorTab.scala - A Cardinal Editor Tab
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad
import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.ui._
import opetopic.js._
import syntax.complex._
import syntax.cardinal._
import markers._
import JsDomFramework._
import JQuerySemanticUI._
import SimpleMarker._

class EditorTab(cOpt: Option[FiniteComplex[OptMarker]] = None) {

  object SketchpadGalleryConfig extends GalleryConfig(
    panelConfig = DefaultPanelConfig,
    width = 1000,
    height = 85,
    spacing = 1500,
    minViewX = Some(60000),
    minViewY = Some(6000),
    spacerBounds = Bounds(0, 0, 600, 600)
  )

  implicit val vf : VisualizableFamily[SimpleMarker] = 
    frameworkFamily(JsDomFramework)

  val editor : CardinalEditor[SimpleMarker] = 
    cOpt match {
      case None => CardinalEditor[SimpleMarker]
      case Some(cSig) => CardinalEditor[SimpleMarker, cSig.N](cSig.value)
    }

  editor.refreshAll

  editor.onSelectAsRoot = (bs: Sigma[editor.CardinalCellBox]) => { activeBox = Some(bs) ; Sketchpad.displayCell }
  editor.onDeselectAll = () => { activeBox = None }

  val uiElement = 
    div(cls := "nofocus", tabindex := 0)(
      editor.element.uiElement
    ).render


  jQuery(uiElement).keypress((e : JQueryEventObject) => {
    e.which match {
      case 101 => editor.extrudeSelection
      case 100 => editor.extrudeDrop
      case 112 => editor.sprout
      case _ => ()
    }
  })

  var activeBox: Option[Sigma[editor.CardinalCellBox]] = None

  implicit val optAFamily : VisualizableFamily[editor.OptA] = 
    VisualizableFamily.optionVisualizableFamily(DefaultGalleryConfig.spacerBounds, editor.v)

  implicit def optAVisualizable[N <: Nat](implicit n: N) : Visualizable[editor.OptA[N], N] = 
    new Visualizable[editor.OptA[N], N] {
      def visualize(o: editor.OptA[N]) : Visualization[N] = 
        optAFamily.visualize(n)(o)
    }

  // import opetopic.pprint._
  // import scala.{Iterator => Iter}

  // implicit val optAPPrint : IndexedPPrint[editor.OptA] = 
  //   new IndexedPPrint[editor.OptA] {
  //     def render[N <: Nat](n: N)(o: editor.OptA[N], c: Config): Iter[String] = 
  //       o match {
  //         case None => Iter("None")
  //         case Some(mk) => Iter("Some(\"" ++ mk.label ++ "\")")
  //       }
  //   }

  import opetopic.Pickler._

  implicit val writer : IndexedWriter[editor.OptA] = 
    toOptionWriter(SimpleMarker.simpleMarkerWriter)

}
