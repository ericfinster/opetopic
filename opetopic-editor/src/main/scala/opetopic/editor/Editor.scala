/**
  * Editor.scala - Playing with a new editor interface
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.editor

import scala.scalajs.js
import js.Dynamic.{literal => obj}
import webix._

import paperjs._

object Editor {

  // Okay, so I think the next step is that we need to associate
  // a new "project" somehow with each instance of the paper view.
  // Then I guess the point is that you have to instantiate your
  // UIFramework class with an encapsulated project so that all
  // rendering is sent to the appropriate canvas.

  def main: Unit = {
    Webix.ready({ () =>

      setupPaperView

      val toolbar =
        obj(
          view = "toolbar",
          paddingY = 0,
          cols = js.Array(
            obj(view = "label", label = "Login", width = 60)
          )
        )

      val menu = 
        obj(
          view = "menu",
          data = js.Array(
            obj(id = "1", value = "Home"),
            obj(id = "2", value = "Sketch"),
            obj(id = "3", value = "Export")
          ),
          css = "blue"
        )

      val layoutObj =
        obj(
          rows = js.Array(
            obj(`type` = "clean",
              cols = js.Array(menu, toolbar)
            ),
            obj(
              cols = js.Array(
                obj(template = "Properties", width = 300),
                obj(view = "resizer"),
                obj(rows = js.Array(
                  obj(view = "paper", id = "editor-paper", canvas = "editor-canvas"),
                  // obj(template = "EditorViewer"),
                  obj(view = "resizer"),
                  obj(template = "FaceViewer")
                ))
              )
            )
          )
        )

      val layout = Webix.ui(layoutObj).asInstanceOf[ui.Layout]
      val paperView = WebixView("editor-paper").asInstanceOf[ui.Paper]

      Paper.setup(paperView.canvas)
      paperView.isSetup = true

      import paperjs.Basic._
      import paperjs.Paths._
      import paperjs.Styling._

      val p0 = Point(20, 20)
      val p1 = Point(50, 50)

      val r = Rect(20, 20, 200, 200)
      val pth = Path.Rectangle(r)
      pth.strokeColor = new Color("black")

      Paper.view.draw()

      println("Ready.")

    })
  }


  // Install a "paper" view element ....
  def setupPaperView: Unit = {

    val webix = js.Dynamic.global.webix
    val doc = js.Dynamic.global.document

    Webix.protoUI(obj(
      name = "paper",
      $init = { (papr: js.Dynamic, config: js.Dynamic) => {

        val elm = doc.createElement("canvas");
        elm.id  = config.canvas;
        papr.canvas = papr.$view.appendChild(elm);
        papr.isSetup = false

      }} : js.ThisFunction1[js.Dynamic, js.Dynamic, Unit],
      $setSize = { (papr: js.Dynamic, x: Double, y: Double) => {

        if (webix.ui.view.prototype.$setSize.call(papr, x,y).asInstanceOf[Boolean]){
          if (papr.isSetup.asInstanceOf[Boolean]) {
            Paper.view.viewSize = paperjs.Basic.Size(x, y)
          } else {
            papr.canvas.width = x
            papr.canvas.height = y
          }
        }

      }} : js.ThisFunction2[js.Dynamic, Double, Double, Unit]
    ), webix.ui.view.asInstanceOf[js.Object])

  }

}
