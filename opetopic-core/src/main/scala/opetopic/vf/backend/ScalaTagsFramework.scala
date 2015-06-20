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
    extends VisualFramework[Double] {

  import bundle._
  import implicits._

  val ^ = svgAttrs

  type ElementType = TypedTag[Builder, Output, FragT]

  class SvgRectangle(val x: Double, val y: Double, val width: Double, val height: Double)
      extends Rectangle {

    def render: Seq[ElementType] = 
      Seq(svgTags.rect(^.x := x.toString, ^.y := y.toString, ^.width := width.toString, ^.height := height.toString))

  }

  class SvgText(val x: Double, val y: Double, val text: String) extends Text {

    def render: Seq[ElementType] =
      Seq(svgTags.text(^.x := x.toString, ^.y := y.toString)(text))

  }

}



