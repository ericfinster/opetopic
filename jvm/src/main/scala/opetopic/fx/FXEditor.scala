/**
  * Editor.scala - A FX-based opetopic editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx

import scalafx.Includes._

import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.input.{KeyEvent, KeyCode}
import scalafx.scene.layout._
import scalafx.scene.control._
import scalafx.scene.paint.Color
import scalafx.geometry._
import scalafx.stage.PopupWindow

import scalafx.scene.control.Alert

import opetopic._
import opetopic.ui._
import opetopic.TypeDefs._
import opetopic.Cardinal._
import opetopic.syntax.complex._

object FXEditor extends JFXApp {

  var tabCount : Int = 1

  var activeEditor : Option[LabeledCellEditor] = None

  def addTab : Unit = {

    val editor = new LabeledCellEditor { thisEditor =>

      onSelectAsRoot = new IndexedOp[FXCardinalMarker] {
        def apply[N <: Nat](n: N)(mk: FXCardinalMarker[N]) : Unit = {

          val nm : FXNeutralMarker[N] = mk.asInstanceOf[FXNeutralMarker[N]]

          propertiesPane.disable = false

          nm.element match {
            case None => labelField.text = ""
            case Some(cl) => labelField.text = cl.label
          }

          applyButton.onAction = () => {

            val str = labelField.text()
            val clr = colorPicker.value()

            if (str == "") {
              nm.element = None
            } else {
              nm.element = Some(ColoredLabel(str, clr))
            }

            thisEditor.deselectAll
            thisEditor.render
            tabPane.requestFocus

          }

          for {
            cmplx <- nm.neutralComplex
          } {

            val facePreview = new LabeledCellViewer(cmplx)
            facePreview.setMaxSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)
            previewPane.children = facePreview
            facePreview.render

          }

        }
      }

    }

    editor.setMaxSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)

    val tab = new Tab {

      text = "Cardinal " ++ tabCount.toString

      content = new StackPane {
        children = editor
        style = "-fx-border-style: solid; -fx-border-size: 2pt; -fx-border-color: blue"
      }

      onSelectionChanged = () => {
        if (selected()) activeEditor = Some(editor)
      }

    }

    tabPane += tab
    tabPane.selectionModel().select(tab)
    editor.render
    tabCount += 1

  }

  val tabPane = new TabPane {
    onKeyPressed = (ev : KeyEvent) => {
      ev.code match {
        case KeyCode.E => for { editor <- activeEditor } { editor.extrudeSelection }
        case KeyCode.D => for { editor <- activeEditor } { editor.extrudeDrop }
        case _ => ()
      }
    }
  }

  val propertiesPane = new GridPane {
    hgap = 10
    vgap = 10
    padding = Insets(10,10,10,10)
    disable = true
  }

  val labelField = new TextField {
    onAction = () => {
      applyButton.fire
    }
  }

  val colorPicker = new ColorPicker
  val applyButton = new Button("Apply")

  propertiesPane.add(new Label("Label: "), 0, 0)
  propertiesPane.add(labelField, 1, 0)
  propertiesPane.add(new Label("Color: "), 0, 1)
  propertiesPane.add(colorPicker, 1, 1)
  propertiesPane.add(applyButton, 1, 2)

  val previewPane = new StackPane {
    // style = "-fx-border-style: solid; -fx-border-size: 2pt; -fx-border-color: red"
    padding = Insets(10, 10, 10, 10)
  }

  val horizontalSplit = new SplitPane {
    orientation = Orientation.HORIZONTAL
    items ++= List(propertiesPane, previewPane)
    dividerPositions = 0.25f
  }

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(tabPane, horizontalSplit)
    dividerPositions = 0.75f
  }

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
        case KeyCode.T => if (ev.isControlDown) addTab
        case _ => ()
      }
    }
  }

  stage = new PrimaryStage {
    title = "FXEditor"
    scene = editorScene
    onShown = () => {
      addTab
    }
  }

}
