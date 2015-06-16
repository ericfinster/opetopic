/**
  * JsSvgText.scala - A react widget for svg text
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

object JsSvgText {

  case class Props(x: Double, y: Double, str: String, ref: js.UndefOr[String], key: js.Any)

  val svgText = Ref[dom.svg.Text]("svgText")

  class Backend(t: BackendScope[Props, _]) {

    def getBounds : js.UndefOr[dom.raw.ClientRect] =
      for { txt <- svgText(t) } yield txt.getDOMNode.getBoundingClientRect

  }

  val component = ReactComponentB[Props]("JsLabel")
    .stateless
    .backend(new Backend(_))
    .render((P, S, B) => {
      text(^.ref := svgText, x := P.x.toString, y := P.y.toString, stroke := "black")(P.str)
    }).build

  def apply(ref: js.UndefOr[String] = "", key: js.Any = {}, x: Double, y: Double, str: String) =
    component.set(key, ref)(Props(x, y, str, ref, key))

}
