/**
  * JsSvgBox.scala - A Simple Box for holding my cells
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

object JsSvgBox {

  // case class Props(x: Double, y: Double, width: Double, height: Double, ref: js.UndefOr[String], key: js.Any)

  // val svgRect = Ref[dom.svg.Rect]("svgRect")

  // class Backend(t: BackendScope[Props, _]) {

  //   // def getBounds : js.UndefOr[dom.raw.ClientRect] =
  //   //   for { txt <- svgText(t) } yield txt.getDOMNode.getBoundingClientRect

  // }

  // val component = ReactComponentB[Props]("JsLabel")
  //   .stateless
  //   .backend(new Backend(_))
  //   .render((P, S, B) => {
  //     rect(^.ref := svgRect, x := P.x.toString, y := P.y.toString, 
  //       width := P.width.toString, height := P.height.toString, 
  //       stroke := "black", fill := "white")
  //   }).build

  // def apply(ref: js.UndefOr[String] = "", key: js.Any = {}, x: Double, y: Double, width: Double, height: Double) =
  //   component.set(key, ref)(Props(x, y, width, height, ref, key))

}
