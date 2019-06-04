/**
  * SimpleViewer.scala - A simple viewer implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.studio.ui

import org.scalajs.dom
import org.scalajs.jquery._
import scalatags.JsDom.all._
import scala.scalajs.js.Dynamic.{literal => lit}

import opetopic._
import opetopic.js._
import opetopic.ui._
import JsDomFramework._
import JQuerySemanticUI._

class SimpleViewer[A: Renderable] extends Component {

  type GalleryType = SimpleActiveGallery[A, JsDomFramework.type]
  type CellType = GalleryType#SimpleActiveCell

  var activeGallery: Option[GalleryType] = None
  var activeComplex: Option[SComplex[A]] = None

  var onSelectAsRoot: CellType => Unit = { _ => () }
  var onHover: CellType => Unit = { _ => () }
  var onUnhover: CellType => Unit = { _ => () }

  var firstPanel: Option[Int] = None
  var lastPanel: Option[Int] = None

  var xOffset: Double = 0
  var yOffset: Double = 0
  var scale: Double = 0.8

  def complex: Option[SComplex[A]] = activeComplex
  def complex_=(oc: Option[SComplex[A]]): Unit = 
    oc match {
      case None => jQuery(uiElement).empty()
      case Some(cc) => {

        val g: GalleryType =
          new SimpleActiveGallery[A, JsDomFramework.type](JsDomFramework)(cc)

        g.hoverCofaces = true // false
        g.firstPanel = firstPanel
        g.lastPanel = lastPanel

        g.onSelectAsRoot = (c: CellType) => onSelectAsRoot(c)
        g.onHover = (c: CellType) => onHover(c)
        g.onUnhover = (c: CellType) => onUnhover(c)

        jQuery(uiElement).empty().append(g.element.uiElement)

        // Rendering sets the viewport size to that of the parent
        g.layoutWidth = bnds => jQuery(uiElement).width.toInt
        g.layoutHeight = bnds => jQuery(uiElement).height.toInt
        g.layoutViewport = bnds => setViewport(bnds)

        activeGallery = Some(g)
        activeComplex = Some(cc)

        g.renderAll

      }
    }

  val uiElement = 
    div().render

  override def setWidth(w: Int): Unit = {
    super.setWidth(w)
    for { g <- activeGallery } {
      g.galleryViewport.width = w
    }
  }

  override def setHeight(h: Int): Unit = {
    super.setHeight(h)
    for {g <- activeGallery } {
      g.galleryViewport.height = h
    }
  }

  def setViewport(bnds: Bounds): Bounds = {

    val halfWidth = bnds.width / 2
    val halfHeight = bnds.height / 2

    val centerX = bnds.x + halfWidth;
    val centerY = bnds.y + halfHeight;

    val newHalfWidth = halfWidth / scale
    val newHalfHeight = halfHeight / scale

    val newX = centerX - newHalfWidth + xOffset
    val newY = centerY - newHalfHeight + yOffset

    Bounds(newX.toInt, newY.toInt, (newHalfWidth * 2).toInt, (newHalfHeight * 2).toInt)

  }

  def refreshViewer: Unit = 
    activeGallery.foreach(_.renderAll)

  def rootAction(f: CellType => Unit): Unit = 
    for {
      g <- activeGallery
      r <- g.selectionRoot
    } { f(r) }

}
