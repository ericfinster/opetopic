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
import webix.ui._

object Editor {

  def main: Unit = {
    Webix.ready({ () =>

      val layoutObj =
        obj(
          cols = js.Array(
            obj(template = "Properties", width = 300),
            obj(view = "resizer"),
            obj(rows = js.Array(
              obj(template = "Editor"),
              obj(view = "resizer"),
              obj(template = "FaceViewer")
            ))
          )
        )

      val layout = Webix.ui(layoutObj).asInstanceOf[Layout]

      println("Ready.")

    })
  }

}
