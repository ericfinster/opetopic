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
import scalafx.scene.control.{Menu, MenuBar, MenuItem, Button, TabPane, SplitPane, Tab, ColorPicker}
import scalafx.scene.layout.BorderPane
import scalafx.scene.layout.HBox
import scalafx.scene.layout.VBox
import scalafx.scene.layout.Region
import scalafx.scene.layout.StackPane
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

  var activeEditor : Option[FXCardinalEditor[ConstString]] = None

  def addTab : Unit = {

    val editor = 
      new FXCardinalEditor[ConstString] {

        def parseStringInput[N <: Nat](str: String) : Polarity[Option[ConstString[N]]] = 
          Neutral(Some(str))

        onSelectAsRoot = new IndexedOp[FXCardinalMarker] {
          def apply[N <: Nat](n: N)(mk: FXCardinalMarker[N]) : Unit = {
            for {
              cmplx <- mk.labelComplex
            } {
              // type IntOpt[K <: Nat] = Polarity[Option[Int]]
              // val test : Complex[IntOpt, N] = cmplx
              // val viewer = new JComplexViewer(cmplx)
            }
          }
        }

      }

    editor.setMaxSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)

    val tab = new Tab {

      text = "Cardinal " ++ tabCount.toString

      content = new StackPane {
        children = editor
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

  // val previewPane = new StackPane {
  //   children = new ColorPicker
  // }

  // val verticalSplit = new SplitPane {
  //   orientation = Orientation.VERTICAL
  //   items ++= List(tabPane, previewPane)
  //   dividerPositions = 0.9f
  // }

  stage = new PrimaryStage {
    title = "FXEditor"
    scene = new Scene {
      root = new BorderPane {
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
      onKeyPressed = (ev : KeyEvent) => {
        ev.code match {
          case KeyCode.T => if (ev.isControlDown) addTab
          case _ => ()
        }
      }
    }
    width = 1300
    height = 700
    onShown = () => {
      addTab
    }
  }

}
