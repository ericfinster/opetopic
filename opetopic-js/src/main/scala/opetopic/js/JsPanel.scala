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

  case class Props(val nesting: Nesting[String, Dim])
  case class State(val initialized: Boolean)

  class Backend(t: BackendScope[Props, State]) extends NestingPanel[String, Double, _1] {

    // Rendering options ..
    def externalPadding: Double = 10.0
    def halfLeafWidth: Double = 5.0
    def halfStrokeWidth: Double = 0.5
    def internalPadding: Double = 10.0

    type MarkerType = JsNestingMarker

    class JsNestingMarker(val label: String, val address: Address[S[Dim]], isExt: Boolean) extends PanelMarker {

      var rootX : Double = 0.0
      var rootY : Double = 0.0

      var halfLabelWidth : Double = 0.0
      var halfLabelHeight : Double = 0.0

      val svgTextRef = Ref.to(JsSvgText.component, label)

      isExternal = isExt

    }

    val nesting: Nesting[JsNestingMarker, Dim] =
      generateMarkers(S(S(Z)))(t.props.nesting)

    val edgeNesting: Nesting[EdgeMarker, _1] = 
      generateEdgeMarkers(S(Z))(nesting)

    def createMarker(lbl: String, addr: Address[S[Dim]], isExt: Boolean) : JsNestingMarker = 
      new JsNestingMarker(lbl, addr, isExt)

    import scalaz.syntax.traverse._
    import scalaz.std.list._

    def labels : List[ReactElement] = 
      nesting.nodes.mapAccumL(1)((i, mk) => {
        (i + 1, JsSvgText(ref = mk.svgTextRef, x = 10, y = (15 * i), str = mk.label.toString))
      })._2

    def elements : List[ReactElement] = 
      nesting.nodes.map(mk => {

        // There seems to be a difference in the calculated position of the
        // label and its actual display position.  I suspect this has to do
        // with some kind of svg text baseline fiddling, but I cannot seem to
        // change this attribute with scala tags (or else chrome is ignoring it).

        val kludge = 12.0

        val labelXPos = mk.x + mk.width - fullStrokeWidth - internalPadding - mk.labelWidth
        val labelYPos = mk.y + mk.height - fullStrokeWidth - internalPadding 

        val labelKludgeYPos = labelYPos - kludge

        List(
          rect(x := mk.x.toString, y := mk.y.toString, width := mk.width.toString, height := mk.height.toString,
            rx := "4", ry := "4", stroke := "black", fill := "white", strokeWidth := fullStrokeWidth.toString, filter := "url(#dropshadow)") : ReactElement,
          JsSvgText(ref = mk.svgTextRef, x = labelXPos, y = labelKludgeYPos, str = mk.label.toString) : ReactElement
          // rect(x := labelXPos.toString, y := (labelYPos - mk.labelHeight).toString, width := mk.labelWidth.toString, height := mk.labelHeight.toString,
          //   stroke := "red", fill := "none") : ReactElement
        )
      }).flatten

    def setLabelSizes : Unit = 
      nesting.foreach(mk => {
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
          val baseMarker = B.nesting.baseValue

          (baseMarker.x - 10).toString ++ " " ++
          (baseMarker.y - 10).toString ++ " " ++
          (baseMarker.width + 20).toString ++ " " ++
          (baseMarker.height + 20).toString
        }

        val els = B.elements

        for { el <- els } { println(React.renderToString(el)) }

        svg(width := "300", height := "300", viewBox := viewboxStr)(
          defs(
            filterTag(id := "dropshadow")(
              fegaussianblur(in := "SourceAlpha", stdDeviation := "3"),
              feoffset(dx := "2", dy := "2", result := "offsetblur"),
              femerge(femergenode, femergenode(in := "SourceGraphic"))
            )
          ),
          els
        )

  // <defs>
  //   <filter id="dropshadow" height="130%">
  //     <feGaussianBlur in="SourceAlpha" stdDeviation="3"/> 
  //     <feOffset dx="2" dy="2" result="offsetblur"/> 
  //     <feMerge> 
  //       <feMergeNode/>
  //       <feMergeNode in="SourceGraphic"/> 
  //     </feMerge>
  //   </filter>
  // </defs>

      } else {
        svg(width := "200", height := "200")(B.labels)
      }
    }).componentDidMount(_.backend.finalizeInit)
    .build

  def apply(nesting : Nesting[String, Dim]) =
    component(Props(nesting))


}
