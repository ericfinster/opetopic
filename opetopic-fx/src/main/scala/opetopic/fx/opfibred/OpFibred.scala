/**
  * OpFibred.scala - The fibred editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx.opfibred

import scalafx.Includes._

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.input._
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.scene.paint.Color
import scalafx.geometry._
import scalafx.stage.PopupWindow
import scalafx.stage.FileChooser

import javafx.scene.{control => jfxsc}
import java.util.function.Consumer

import opetopic._
import opetopic.ui._
import opetopic.TypeDefs._
import opetopic.Cardinal._
import opetopic.syntax.complex._
import opetopic.syntax.cardinal._

object OpFibred extends JFXApp {

  val emptyTreeItem = 
    new TreeItem[Context](Context.Empty)

  val contextTreeView = 
    new TreeView[Context](emptyTreeItem) {
      showRoot = true
      cellFactory = (tv : TreeView[Context]) => {
        new jfxsc.TreeCell[Context] {
          override def updateItem(item: Context, empty: Boolean) : Unit = {
            super.updateItem(item, empty)
            if (item != null) {
              setText(item.name)
            }
          }

          setOnMouseClicked((ev: MouseEvent) => {
            if (ev.clickCount > 1) {
              val item = getTreeItem
              if (item != null) {
                val cntxt = item.getValue
                if (cntxt != null) {

                  val newBuilderDialog = new TextInputDialog
                  newBuilderDialog.setTitle("New Context Builder")
                  newBuilderDialog.setHeaderText("New Context Name")
                  newBuilderDialog.setContentText("Context name: ")

                  for {
                    name <- newBuilderDialog.showAndWait
                  } {
                    addBuilder(name, cntxt)
                  }
                }
              }
            } 
          })
        }
      }
    }


  val tabPane = new TabPane {
    onKeyPressed = (ev : KeyEvent) => {
      ev.code match {
        case KeyCode.E => for { builder <- activeBuilder } { builder.editor.extrudeSelection }
        case KeyCode.D => for { builder <- activeBuilder } { builder.editor.extrudeDrop }
        case _ => ()
      }
    }
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items ++= List(contextTreeView, tabPane)
    dividerPositions = 0.15f
  }

  val contextTab = new Tab {
    text = "Context"
  }

  val faceTab = new Tab {
    text = "Faces"
  }

  val previewPane = new TabPane {
    tabs ++= List(contextTab, faceTab)
    side = Side.BOTTOM
    tabClosingPolicy = TabPane.TabClosingPolicy.UNAVAILABLE
  }

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(horizontalSplit, previewPane)
    dividerPositions = 0.75f
  }

  val fileChooser = new FileChooser

  val borderPane = new BorderPane {
    top = new MenuBar {
      useSystemMenuBar = true
      menus = List(
        new Menu("File") {
          items = List(
            new MenuItem("Exit") {
              onAction = () => {
                scalafx.application.Platform.exit
              }
            }
          )
        }
      )
    }
    center = verticalSplit
  }

  val editorScene = new Scene(borderPane, 1300, 700) {
    onKeyPressed = (ev : KeyEvent) => {
      ev.code match {
        // case KeyCode.T => if (ev.isControlDown) addTab
        case _ => ()
      }
    }
  }

  stage = new PrimaryStage {
    title = "OpFibred Editor"
    scene = editorScene
  }

  //============================================================================================
  // HELPER METHODS
  //

  var activeBuilder : Option[ContextBuilder] = None

  def addBuilder(name: String, cntxt: Context) : Unit = {

    val builder = new ContextBuilder(name, cntxt)

    val tab = new Tab {
      text = name
      content = builder
      onSelectionChanged = () => {
        if (selected()) activeBuilder = Some(builder)
      }

    }

    tabPane.tabs += tab

  }

}
