/**
  * FXDialogs.scala - Some custom dialog boxes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx

import javafx.util.Callback

import javafx.scene.control.Dialog
import javafx.scene.control.ButtonType
import javafx.scene.control.TextArea

object FXDialogs {

  class CodeDisplayDialog(code: String) extends Dialog[Unit] {

    setTitle("Code Output")
    setHeaderText("Some code for you.")

    getDialogPane.getButtonTypes.addAll(ButtonType.OK)

    setResultConverter(new Callback[ButtonType, Unit] {
      def call(bt: ButtonType) : Unit = ()
    })

    val textArea = new TextArea(code)
    textArea.setWrapText(true)

    getDialogPane.setContent(textArea)

  }

  // class PropertiesDialog extends Dialog[CellProperties] {

  //   setTitle("Cell Properties")
  //   setHeaderText("Cell Properties")

  //   getDialogPane.getButtonTypes.addAll(ButtonType.OK, ButtonType.CANCEL)

  //   val cb = new Callback[ButtonType, CellProperties] { 
  //     def call(bt: ButtonType) : CellProperties = 
  //       bt match {
  //         case ButtonType.OK => CellProperties(labelField.getText, colorPicker.getValue)
  //         case ButtonType.CANCEL => CellProperties("", Color.WHITE)
  //       }
  //   }

  //   setResultConverter(cb)

  //   val grid = new GridPane
  //   grid.setHgap(10)
  //   grid.setVgap(10)

  //   val labelField = new TextField
  //   val colorPicker = new ColorPicker

  //   grid.add(new Label("Label: "), 0, 0)
  //   grid.add(labelField, 1, 0)
  //   grid.add(new Label("Color: "), 0, 1)
  //   grid.add(colorPicker, 1, 1)

  //   getDialogPane.setContent(grid)

  // }



}
