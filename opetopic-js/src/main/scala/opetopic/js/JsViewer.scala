/**
  * JsViewer.scala - JavaScript Viewer Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import japgolly.scalajs.react._

import vdom.prefix_<^._
import vdom.SvgTags._
import vdom.SvgAttrs._

import scala.scalajs._
import org.scalajs.dom

import opetopic._
import opetopic.ui._

import TypeDefs._
import syntax.complex._

object JsViewer {

  // Okay, here's the thing.  We're just going to take this a step at 
  // at time.  You're coming up on a new backend for this stuff, just
  // write a couple versions and see where you go.

  // The first version will just be a static, no frills

  trait BoxSpec[N <: Nat] {
    def color : Int
    def label : String
  }

  case class Props(complex: FiniteComplex[BoxSpec])
  case class State(initialized: Boolean)

  class Backend(t: BackendScope[Props, State]) extends Renderer[Double] {

    implicit val isNumeric: Numeric[Double] = 
      implicitly[Numeric[Double]]

    def externalPadding: Double = 10.0
    def halfLeafWidth: Double = 5.0
    def halfStrokeWidth: Double = 1.0
    def internalPadding: Double = 10.0

    class RenderedBox[N <: Nat](spec: BoxSpec[N], ext: Boolean) extends RenderMarker {

      val ref = Ref[dom.svg.Text]("<<" ++ spec.label ++ ">>.text")

      def textBounds : dom.raw.ClientRect = 
        ref(t).get.getDOMNode.getBoundingClientRect

      def halfLabelHeight: Double = textBounds.width / 2
      def halfLabelWidth: Double = textBounds.height / 2

      def isExternal: Boolean = ext

      var outgoingEdgeMarker : Option[RenderMarker] = None

      var rootX : Double = 0.0
      var rootY : Double = 0.0

    }

    // Mmmm.  So the next thing to realize is that these get broken up
    // by dimension ....

  }

  val component = ReactComponentB[Props]("JsViewer") 
    .initialState(State(false))
    .backend(new Backend(_))
    .render((P, S, B) => {

      if (S.initialized) {
        svg(width := "100", height := "100")
      } else {
        svg(width := "100", height := "100")
      }

    }).build

}
