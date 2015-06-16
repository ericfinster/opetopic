/**
  * JsPanel.scala - Javascript Panel Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import japgolly.scalajs.react._

import vdom.prefix_<^._
import vdom.SvgTags._
import vdom.SvgAttrs._

import scala.scalajs.{js => sjs, _}
import org.scalajs.dom

import opetopic._
import opetopic.newui._

import TypeDefs._
import syntax.tree._
import syntax.nesting._

object JsPanel {

  type Dim = _2

  case class Props(val nesting: Nesting[Int, Dim])
  case class State(val initialized: Boolean)

  class Backend(t: BackendScope[Props, State]) extends NestingPanel[Int, Double, _1](t.props.nesting) {

    // Rendering options ..
    def externalPadding: Double = 10.0
    def halfLeafWidth: Double = 5.0
    def halfStrokeWidth: Double = 1.0
    def internalPadding: Double = 10.0

    type MarkerType = JsNestingMarker

    class JsNestingMarker(val label: Int) extends RenderMarker {

      var rootX : Double = 0.0
      var rootY : Double = 0.0

      var halfLabelWidth : Double = 0.0
      var halfLabelHeight : Double = 0.0

      val svgTextRef = Ref.to(JsSvgText.component, "svgTextRef")

    }

    def createMarker(lbl: Int, addr: Address[S[Dim]], isExt: Boolean) : JsNestingMarker = 
      new JsNestingMarker(lbl)

    import scalaz.syntax.traverse._
    import scalaz.std.list._

    def labels : List[ReactElement] = 
      markerNesting.nodes.mapAccumL(1)((i, mk) => {
        (i + 1, JsSvgText(ref = mk.svgTextRef, x = 10, y = (15 * i), str = mk.label.toString))
      })._2

    def elements : List[ReactElement] = 
      markerNesting.nodes.map(mk => {

        val labelXPos = mk.x + mk.width - strokeWidth - internalPadding - mk.labelWidth
        val labelYPos = mk.y + mk.height - strokeWidth - internalPadding //- mk.labelHeight

        List(
          rect(x := mk.x.toString, y := mk.y.toString, width := mk.width.toString, height := mk.height.toString,
            rx := "4", ry := "4", stroke := "black", fill := "white") : ReactElement,
          JsSvgText(ref = mk.svgTextRef, x = labelXPos, y = labelYPos, str = mk.label.toString) : ReactElement
        )
      }).flatten

    def setLabelSizes : Unit = 
      markerNesting.foreach(mk => {
        for {
          text <- mk.svgTextRef(t)
          rect <- text.backend.getBounds
        } {
          println("Setting size for " ++ mk.label.toString ++ " to (" ++ rect.width.toString ++ ", " ++ rect.height.toString ++ ")")
          mk.halfLabelWidth = rect.width / 2
          mk.halfLabelHeight = rect.height / 2
        }
      })

    def finalizeInit : Unit = {
      setLabelSizes

      for {
        _ <- layout
      } {
        println("Layout finished ...")
        t.setState(State(true))
      }
    }

  }

  val component = ReactComponentB[Props]("JsPanel") 
    .initialState(State(false))
    .backend(new Backend(_))
    .render((P, S, B) => {
      if (S.initialized) {

        val viewboxStr : String = {
          val baseMarker = B.markerNesting.baseValue

          (baseMarker.x - 10).toString ++ " " ++
          (baseMarker.y - 10).toString ++ " " ++
          (baseMarker.width + 20).toString ++ " " ++
          (baseMarker.height + 20).toString
        }

        svg(width := "300", height := "300", viewBox := viewboxStr)(B.elements)
      } else {
        svg(width := "200", height := "200")(B.labels)
      }
    }).componentDidMount(_.backend.finalizeInit)
    .build

  def apply(nesting : Nesting[Int, Dim]) =
    component(Props(nesting))


}
