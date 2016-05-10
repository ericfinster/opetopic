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
import JsDomFramework.{SimpleActiveGallery => _, _}
import JQuerySemanticUI._

class JsStableViewer[A: Renderable] {

  var viewerWidth : Int = 0
  var viewerHeight : Int = 0

  val gallery = new SimpleActiveGallery[A, JsDomFramework.type](JsDomFramework)

  type ConstA[N <: Nat] = A

  def loadComplex(fc: FiniteComplex[ConstA]): Unit = {

      for {
        _ <- gallery.ComplexImporter.fromComplex(fc.n)(fc.value)
      } {
        gallery.renderAll
      }

  }

  def loadFace(c: Cell[A, _]): Unit = {

    gallery.myPanels.clear

    for {
      face <- c.face(gallery.SimpleFactory)
    } {

      face.targets.reverse.foreach(t => {
        gallery.myPanels += gallery.SimpleActivePanel(t)
      })

      gallery.renderAll

    }
  }

  val uiElement = 
    div(style := "min-height: 200px").render

  jQuery(uiElement).append(gallery.element.uiElement)

}
