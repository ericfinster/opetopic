/**
  * JsEditor.scala - A Cardinal Editor in JavaScript
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

object JsEditor extends js.JSApp {

  object JsCanvas {

    case class Props(width: Int, height: Int)
    case class State(initializing: Boolean)

    class Backend(t: BackendScope[Props, State]) {

      var labelWidth : Double = 0.0
      var labelHeight : Double = 0.0

      def afterMounted : Unit = {
        for {
          lbl <- theLabel(t)
          rect <- lbl.backend.getBounds
        } {
          println("My label has dimensions: (" ++ rect.width.toString ++ ", " ++ rect.height.toString ++ ")")
          println("Canvas is mounted.")
          labelWidth = rect.width
          labelHeight = rect.height
          t.setState(State(false))
        }
      }

    }

    val theLabel = Ref.to(JsSvgText.component, "theLabel")

    val component = ReactComponentB[Props]("JsCanvas")
      .initialState(State(true))
      .backend(new Backend(_))
      .render((P, S, B) => {
        if (S.initializing) {
          println("In initialization pass ...")
          svg(width := P.width.toString, height := P.height.toString)(
            JsSvgText(ref = theLabel, x = 10, y = 50, str = "This is some SVG text")
          )
        } else {
          println("In finialization pass ...")

          svg(width := P.width.toString, height := P.height.toString)(
            rect(x := "5", y := (50 - B.labelHeight).toString, width := (B.labelWidth + 10).toString, height := (B.labelHeight + 5).toString, rx := "4", ry := "4",
              stroke := "black", strokeWidth := "2", fill := "white"
            ),
            JsSvgText(ref = theLabel, x = 10, y = 50, str = "This is some SVG text")
          )
        }
      }).componentDidMount(_.backend.afterMounted)
      .build

    def apply(width: Int, height: Int) = 
      component(Props(width, height))

  }

  def main : Unit = {

    val mainContent = dom.document.getElementById("main")

    if (mainContent != null) {
      import opetopic.Examples._
      import opetopic.syntax.nesting._
      React.render(JsPanel(exotic), mainContent)
    }

  }

}
