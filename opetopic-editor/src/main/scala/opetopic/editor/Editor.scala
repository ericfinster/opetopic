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

import opetopic._
import opetopic.ui._
import opetopic.js.JsDomFramework

object Editor {

  type EditorType = CardinalEditor[SimpleMarker, JsDomFramework.type]

  def main: Unit = {
    Webix.ready({ () =>

      val toolbar =
        obj(
          view = "toolbar",
          paddingY = 0,
          cols = js.Array(
            obj(view = "label", label = "Login", width = 60)
          )
        )

      val properties =
        obj(
          view = "property",
          id = "property-view",
          width = 300,
          elements = js.Array(
            obj(label = "Properties", `type` = "label"),
            obj(label = "Label", `type` = "text"),
            obj(label = "Stroke", `type` = "color", cols = 20, rows = 20),
            obj(label = "Fill", `type` = "color", cols = 20, rows = 20)
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
                obj(template = "Cell Stack", width = 300),
                obj(
                  rows = js.Array(
                    obj(template = "EditorViewer", id = "editor-view"),
                    obj(view = "resizer"),
                    obj(
                      cols = js.Array(
                        obj(template = "FaceViewer"),
                        properties
                      )
                    )
                  )
                )
              )
            )
          )
        )

      val layout = Webix.ui(layoutObj).asInstanceOf[webix.ui.Layout]
      val editorView = WebixView("editor-view").asInstanceOf[webix.ui.Template]

      // Now create an editor
      val editor = CardinalEditor[SimpleMarker, JsDomFramework.type](JsDomFramework)

      editor.layoutWidth = bnds => editorView.$width
      editor.layoutHeight = bnds => editorView.$height

      editorView.setContent(editor.element.uiElement)

      editorView.attachEvent("onViewResize", () => {
        editor.galleryViewport.width = editorView.$width
        editor.galleryViewport.height = editorView.$height
      })

      Webix.UIManager.addHotKey("e", () => { editor.extrudeSelection }, editorView)
      Webix.UIManager.addHotKey("d", () => { editor.loopAtSelection }, editorView)
      Webix.UIManager.addHotKey("s", () => { editor.sproutAtSelection }, editorView)
      
      editor.renderAll

      println("Ready.")

    })
  }

}
