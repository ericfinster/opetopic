/**
  * ScalaTagsFramework.scala - Visual Framework Implementation for ScalaTags
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.vf.backend

import scalatags.generic._

import opetopic._
import opetopic.vf._
import TypeDefs._

abstract class ScalaTagsFramework[Builder, Output <: FragT, FragT](val bundle: Bundle[Builder, Output, FragT]) 
    extends VisualFramework[Double] with PanelDeps[Double] {

  import bundle._
  import implicits._

  val ^ = svgAttrs

  type ElementType = TypedTag[Builder, Output, FragT]

  //============================================================================================
  // CONTRUCTORS
  //

  def rect(x: Double, y: Double, width: Double, height: Double) : Rectangle = 
    new SvgRectangle(x, y, width, height)

  def text(x: Double, y: Double, txt: String) : Text = 
    new SvgText(x, y, txt)

  //============================================================================================
  // RECTANGLES
  //

  class SvgRectangle(val x: Double, val y: Double, val width: Double, val height: Double)
      extends Rectangle {

    def render: Seq[ElementType] = 
      Seq(svgTags.rect(^.x := x.toString, ^.y := y.toString, ^.width := width.toString, ^.height := height.toString))

  }

  //============================================================================================
  // TEXT
  //

  class SvgText(val x: Double, val y: Double, val text: String) extends Text {

    def render: Seq[ElementType] =
      Seq(svgTags.text(^.x := x.toString, ^.y := y.toString)(text))

  }

}

object ScalatagsTextFramework extends ScalaTagsFramework(scalatags.Text)

