/**
  * ActiveComplex.scala - An active stable complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.ListBuffer

import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.std.option._

import opetopic._
import opetopic.ui._

trait HasActiveComplex extends HasVisualCells { self : ActiveFramework =>

  import isNumeric._

  case class PanelConfig(
    val internalPadding : Size = fromInt(400),
    val externalPadding : Size = fromInt(600),
    val decorationPadding : Size = fromInt(400),
    val leafWidth : Size = fromInt(200),
    val strokeWidth : Size = fromInt(100),
    val cornerRadius : Size = fromInt(200),
    val manageViewport : Boolean = false
  )

  class ActiveComplex[A](config: PanelConfig, labelRender: Option[A] => BoundedElement) {

    // The base cell buffer
    val baseCells: ListBuffer[ActiveVisualCell] = ListBuffer()

    class ActiveVisualCell extends VisualCell[A, ActiveVisualCell] { thisCell =>

      //
      //  Cell Structure Variables
      //

      var dim: Int = 0
      var label: Option[A] = None

      var canopy: Option[CellTree] = None
      var container: Option[ActiveVisualCell] = None

      var target : Option[ActiveVisualCell] = None
      var sourceTree: Option[CellTree] = None

      var incoming: Option[ActiveVisualCell] = None
      var outgoing: Option[ActiveVisualCell] = None

      //
      //  Visual Settings
      //

      def internalPadding : Size = config.internalPadding
      def externalPadding : Size = config.externalPadding
      def decorationPadding : Size = config.decorationPadding
      def leafWidth : Size = config.leafWidth
      def strokeWidth : Size = config.strokeWidth
      def cornerRadius : Size = config.cornerRadius

      // Here we put the actual active ui elements which we wish to save,
      // along with any routines for actually creating them.

      var labelElement: BoundedElement = labelRender(label)

      //
      //  Visual Elements
      //

      val boxRect = {
        val r = rect
        r.r = cornerRadius
        r.strokeWidth = strokeWidth
        r
      }

      val edgePath = {
        val p = path
        p.stroke = "black"
        p.strokeWidth = strokeWidth
        p.fill = "none"
        makeMouseInvisible(p)
        p
      }

    }

    //
    //  A Builder Implementation for importing
    //

    object ActiveComplexBuilder extends ComplexBuilder[A, ActiveVisualCell] {

      def newCell(opt: Option[A]): ActiveVisualCell = {
        val cell = new ActiveVisualCell
        cell.label = opt
        cell
      }

      def newCell(opt: Option[A], d: Nat): ActiveVisualCell = {
        val cell = newCell(opt)
        cell.dim = natToInt(d)
        cell
      }

      def registerBaseCell(cell: ActiveVisualCell): Unit = {
        baseCells += cell
      }

    }

  }


}
