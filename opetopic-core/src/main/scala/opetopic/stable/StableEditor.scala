/**
  * StableEditor.scala - A Stable Opetopic Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.Buffer

import opetopic._
import opetopic.ui._

class StableEditor[A : Renderable, F <: ActiveFramework](frmwk: F) 
    extends ActiveStableGallery[A, F](frmwk) { thisEditor => 

  import framework._
  import isNumeric._

  type CellType = EditorCell
  type PanelType = EditorPanel

  val editorPanels: Buffer[EditorPanel]= Buffer.empty
  def panels = editorPanels.toList

  //
  //  Visual Options
  //

  var internalPadding : Size = fromInt(400)
  var externalPadding : Size = fromInt(600)
  var leafWidth : Size = fromInt(200)
  var strokeWidth : Size = fromInt(100)
  var cornerRadius : Size = fromInt(200)

  //
  //  Gallery Options
  //

  var width: Size = fromInt(900)
  var height: Size = fromInt(300)
  var minViewX: Option[Size] = None
  var minViewY: Option[Size] = None
  var spacing: Size = fromInt(2000)
  var manageViewport : Boolean = false

  abstract class EditorPanel extends ActiveStablePanel 
  abstract class EditorCell extends ActiveCell

//     class NeutralCell(var value: Option[A]) extends EditorCell {
//       def label: Polarity[Option[A]] = Neutral(value)
//       def renderCell: Unit = ()
//       def renderEdge: Unit = ()
//       def labelElement: BoundedElement = ???
//     }

//     class PositiveCell extends EditorCell {
//       val label: Polarity[Option[A]] = Positive()
//       def renderCell: Unit = ()
//       def renderEdge: Unit = ()
//       def labelElement: BoundedElement = ???
//     }


}

