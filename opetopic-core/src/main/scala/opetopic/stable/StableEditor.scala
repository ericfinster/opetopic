/**
  * StableEditor.scala - A Stable Opetopic Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.ListBuffer

import opetopic._
import opetopic.ui._

trait HasStableEditor extends HasStableGallery { self : ActiveFramework =>

  class StableEditor[A](
    val config: StableGalleryConfig, 
    val renderer: A => BoundedElement
  ) extends StableGallery[Polarity[Option[A]]] {

    type CellType = EditorCell
    type PanelType = EditorPanel

    val panels: ListBuffer[EditorPanel] = ListBuffer()

    class EditorPanel extends Panel {

      val base: PositiveCell = new PositiveCell
      def baseCell: EditorCell = base

      def element: Element = ???

    }

    abstract class EditorCell extends VisualCell {


    }

    class NeutralCell(var value: Option[A]) extends EditorCell {
      def label: Polarity[Option[A]] = Neutral(value)
      def renderCell: Unit = ()
      def renderEdge: Unit = ()
      def labelElement: BoundedElement = ???
    }

    class PositiveCell extends EditorCell {
      val label: Polarity[Option[A]] = Positive()
      def renderCell: Unit = ()
      def renderEdge: Unit = ()
      def labelElement: BoundedElement = ???
    }

  }

}
