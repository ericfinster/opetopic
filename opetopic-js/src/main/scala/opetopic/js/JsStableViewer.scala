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
import opetopic.ui._
import JsDomFramework._
import JQuerySemanticUI._

class JsStableViewer[A: Renderable](c: SComplex[A]) {

  type GalleryType = SimpleActiveGallery[A, JsDomFramework.type]

  val gallery: GalleryType =
    new SimpleActiveGallery[A, JsDomFramework.type](JsDomFramework)(c)

  val uiElement = 
    div(style := "min-height: 200px").render

  jQuery(uiElement).append(gallery.element.uiElement)

}
