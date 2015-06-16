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
import scalafx.stage.FileChooser
import scalafx.scene.input.MouseEvent

import scalafx.scene.control.Alert

import opetopic._
import opetopic.ui._
import opetopic.TypeDefs._
import opetopic.Cardinal._
import opetopic.syntax.complex._
import opetopic.syntax.cardinal._

object FXEditor extends JFXApp {

  var tabCount : Int = 1

  var activeEditor : Option[LabeledCellEditor] = None
  var activePreview : Option[LabeledCellViewer] = None

  def addTab : Unit = 
    addTab(Complex[ColoredLabel.LabelOpt]() >> Obj(None))


  def addTab(cmplx: FiniteComplex[ColoredLabel.LabelOpt]) : Unit = {
    addTabWithCardinal(Cardinal.complexToCardinal(cmplx.n)(cmplx.value)._1)
  }

  def addTabWithCardinal(cardinal: FiniteCardinal[ColoredLabel.LabelOpt]) : Unit = {

    val editor = new LabeledCellEditor(cardinal) { thisEditor =>

      onSelectAsRoot = new IndexedOp[FXCardinalMarker] {
        def apply[N <: Nat](n: N)(mk: FXCardinalMarker[N]) : Unit = {

          val nm : FXNeutralMarker[N] = mk.asInstanceOf[FXNeutralMarker[N]]

          for {
            cmplx <- nm.neutralComplex
          } {

            val facePreview = new LabeledCellViewer(cmplx)
            facePreview.setMaxSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)
            previewPane.children = facePreview
            activePreview = Some(facePreview)
            facePreview.render

          }

        }
      }

    }

    editor.setMaxSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)

    val editorStack = new StackPane {
      // style = "-fx-border-style: solid; -fx-border-size: 2pt; -fx-border-color: blue"
      children = editor
      handleEvent(MouseEvent.MouseClicked)((ev: MouseEvent) => {
        editor.deselectAll
      })
    }

    val tab = new Tab {
      text = "Cardinal " ++ tabCount.toString
      content = editorStack
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
        case KeyCode.P => for { editor <- activeEditor } { editor.sprout }
        case KeyCode.X => 
          for {
            editor <- fromOpt(activeEditor)
            sel <- fromOpt(editor.selection)
            pd <- sel.asPastingDiagram
          } {

            @natElim
            def addTabFromPastingDiagram[N <: Nat](n: N)(pd: Tree[editor.MarkerType[N], N]) : ShapeM[Unit] = {
              case (Z, pd) => fail("Not implemented")
              case (S(p: P), pd) => {
                type CL[N <: Nat] = ColoredLabel.LabelOpt[N]

                for {
                  cardinal <- Cardinal.pasteToCardinal(p)(Tree.map(pd)(_.asInstanceOf[editor.NeutralMarker[S[P]]].neutralComplex.get))(
                    new Discriminator[CL] {
                      def apply[N <: Nat](n: N)(s: CL[N], t: CL[N]): ShapeM[CL[N]] = 
                        succeed(s)
                    }
                  )
                } yield addTabWithCardinal(cardinal)
              }
            }

            for {
             _ <- addTabFromPastingDiagram(sel.dim)(pd)
            } yield ()
          }
        case _ => ()
      }
    }
  }

  val previewPane = new StackPane {
    style = "-fx-border-style: solid; -fx-border-size: 2pt; -fx-border-color: grey"
    padding = Insets(10, 10, 10, 10)
  }

  VBox.setVgrow(previewPane, Priority.Always)

  val getCodeButton = new Button("Get Code") {
    onAction = () => {
      for {
        viewer <- activePreview
      } {
        val code = viewer.labelComplex.value.toString
        val codeViewer = new FXDialogs.CodeDisplayDialog(code)
        codeViewer.showAndWait
      }
    }
  }

  val getJsonButton = new Button("Get Json") {
    onAction = () => {
      for {
        viewer <- activePreview
      } {

        val complex : FiniteComplex[ColoredLabel.LabelOpt] = 
          viewer.labelComplex

        import upickle._
        import Complex._

        val jsonViewer = new FXDialogs.CodeDisplayDialog(write(complex.value))
        jsonViewer.showAndWait

      }
    }
  }

  // val getSvgButton = new Button("Get Svg") {
  //   onAction = () => {
  //     for {
  //       viewer <- activePreview
  //     } {
  //       val svgViewer = new FXDialogs.CodeDisplayDialog(viewer.toSvg.toString)
  //       svgViewer.showAndWait
  //     }
  //   }
  // }

  val toCardinalButton = new Button("To Cardinal") {
    onAction = () => {
      for {
        viewer <- activePreview
      } {

        val complex : FiniteComplex[ColoredLabel.LabelOpt] = 
          viewer.labelComplex

        addTab(complex)

      }
    }
  }

  val buttonTray = new HBox {
    children = List(getCodeButton, getJsonButton, toCardinalButton)
    spacing = 10
  }

  val previewVBox = new VBox {
    children = List(previewPane, buttonTray)
    padding = Insets(10, 10, 10, 10)
    spacing = 10
  }

  val verticalSplit = new SplitPane {
    orientation = Orientation.VERTICAL
    items ++= List(tabPane, previewVBox)
    dividerPositions = 0.75f
  }

  val fileChooser = new FileChooser

  def onSaveComplex : Unit = 
    for {
      viewer <- activePreview
    } {

      fileChooser.setTitle("Save Complex")

      val file = fileChooser.showSaveDialog(editorScene.getWindow)

      if (file != null) {

        val complex : FiniteComplex[ColoredLabel.LabelOpt] =
          viewer.labelComplex

        import upickle._
        import Complex._

        val writer = new java.io.PrintWriter(file)
        writer.write(write(complex.value))
        writer.close

      }

    }

  def onLoadComplex : Unit = {

    fileChooser.setTitle("Load Complex")

    val file = fileChooser.showOpenDialog(editorScene.getWindow)

    if (file != null) {

      val reader = new java.io.FileReader(file)
      val src = io.Source.fromFile(file, "UTF-8")
      val json = src.getLines.mkString
      src.close

      // Now, we should try to read the json ...

      import upickle._
      import Complex._

      val complex : FiniteComplex[ColoredLabel.LabelOpt] =
        read[FiniteComplex[ColoredLabel.LabelOpt]](json)

      addTab(complex)

    }
  }

  val borderPane = new BorderPane {
    top = new MenuBar {
      useSystemMenuBar = true
      menus = List(
        new Menu("File") {
          items = List(
            new MenuItem("Save Complex") {
              onAction = () => { onSaveComplex }
            },
            new MenuItem("Load Complex") {
              onAction = () => { onLoadComplex }
            },
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
