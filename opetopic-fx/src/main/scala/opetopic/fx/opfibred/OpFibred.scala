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
import opetopic.oldui._
import opetopic.Cardinal._
import opetopic.syntax.complex._
import opetopic.syntax.cardinal._

object OpFibred extends JFXApp {

  val tabPane = new TabPane 

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
    center = tabPane
  }

  val editorScene = new Scene(borderPane, 1300, 700) {
    onKeyPressed = (ev : KeyEvent) => {
      ev.code match {
        case KeyCode.T => if (ev.isControlDown) addTab
        case KeyCode.E => for { stack <- activeStack } { stack.editor.extrudeSelection }
        case KeyCode.D => for { stack <- activeStack } { stack.editor.extrudeDrop }
        case KeyCode.X => for { stack <- activeStack } { stack.extendByContext }
        case _ => ()
      }
    }
  }

  stage = new PrimaryStage {
    title = "OpFibred Editor"
    scene = editorScene
    onShown = () => addTab
  }

  //============================================================================================
  // HELPER METHODS
  //

  var activeStack : Option[ContextStack] = None

  def addTab : Unit = {

    val stack = new ContextStack
    val tab = new Tab {
      text = "Context"
      content = stack
      onSelectionChanged = () => {
        if (selected()) activeStack = Some(stack)
      }
    }

    tabPane.tabs += tab

  }

}
