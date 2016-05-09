/**
  * JsStableViewer.scala - Stable Complex Viewer
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.stable._
import JsDomFramework._
import JQuerySemanticUI._

class JsStableViewer {

  type FType = JsDomFramework.type

  var viewerWidth : Int = 0
  var viewerHeight : Int = 0

  def renderOption: Option[String] => BoundedElement = {
    case None => spacer(Bounds(0,0,600,600))
    case Some(str) => text(str)
  }

  val gallery = new ActiveStableGallery(StableGalleryConfig(), renderOption)

  type OptString[N <: Nat] = Option[String]

  def loadComplex(fc: FiniteComplex[OptString]): Unit = {

      for {
        _ <- gallery.StableBuilder.fromComplex(fc.n)(fc.value)
      } {
        gallery.initialize
        gallery.refreshAll

        println("Loaded complex.")
      }

    jQuery(uiElement).empty().append(gallery.uiElement)

  }

  val uiElement = 
    div(style := "min-height: 200px").render

}
