/**
  * StableEditor.scala - A Stable Cardinal Editor
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scalaz.Traverse
import scalaz.syntax.traverse._

import opetopic._
import opetopic.ui._

class StableEditor[A : Renderable, F <: ActiveFramework](frmwk: F)(c: SCardinal[Option[A]])
    extends ActiveStableGallery[Polarity[Option[A]], F](frmwk) {

  import framework._
  import isNumeric._

  type BoxType = EditorCell
  type EdgeType = EditorCell

  type PanelType = EditorPanel

  type OptA = Option[A]
  type PolOptA = Polarity[Option[A]]

  val renderer : Renderable[PolOptA] = 
    Renderable[PolOptA]

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

  //============================================================================================
  // EDITOR DATA
  //

  var panels : Suite[PanelType] = buildPanels(c)

  def cardinal: SCardinal[NeutralCell] = 
    Traverse[Suite].map(panels)(_.cardinalNesting)

  def complex: SComplex[EditorCell] = 
    Traverse[Suite].map(panels)(_.cellNesting)

  //============================================================================================
  // INITIALIZATION
  //

  def buildPanels(c: SCardinal[OptA]): Suite[PanelType] = 
    c match {
      case ||(cn) => ???
      case tl >> hd => ???
    }

  //============================================================================================
  // PANEL IMPLEMENTATION
  //

  abstract class EditorPanel(
    val dim: Int
  ) extends ActiveStablePanel {

    var cardinalNesting: SCardNst[NeutralCell] = ???

    val positiveCell: PositiveCell = new PositiveCell(dim)
    val negativeCell: NegativeCell = new NegativeCell(dim)

    def cellNesting: SNesting[EditorCell] = 
      cardinalNesting.toNesting(
        positiveCell, 
        negativeCell
      )

  }

  //============================================================================================
  // CELL IMPLEMENTATIONS
  //

  abstract class EditorCell extends ActiveBox with ActiveEdge {

    def onClick: Unit = ()
    def onMouseOver: Unit = ()
    def onMouseOut: Unit = ()

  }

  class NeutralCell(
    val dim: Int,
    var optA: Option[A],
    var isExternal: Boolean
  ) extends EditorCell {

    // FIXME!!
    def address: SAddr = Nil

    // Label data
    def label: PolOptA = Neutral(optA)
    def labelBE: BoundedElement = renderer.render(framework)(label)
    def labelElement: Element = labelBE.element
    def labelBounds: Bounds = labelBE.bounds

  }

  abstract class PolarizedCell extends EditorCell {
    val address: SAddr = Nil
    def polarityElement: BoundedElement
    def labelElement = polarityElement.element
    def labelBounds = polarityElement.bounds
  }

  class NegativeCell(val dim: Int) extends PolarizedCell {

    val label: PolOptA = Negative()
    val isExternal: Boolean = true

    val polarityElement: BoundedElement = 
      renderer.render(framework)(label)

  }

  class PositiveCell(val dim: Int) extends PolarizedCell {

    val label: PolOptA = Positive()
    val isExternal: Boolean = false

    val polarityElement: BoundedElement = 
      renderer.render(framework)(label)

  }

}

object StableEditor {

  def apply[A, F <: ActiveFramework](f: F)(implicit r: Renderable[A]): StableEditor[A, F] = 
    new StableEditor(f)(SCardinal(None))

  def apply[A, F <: ActiveFramework](f: F)(c: SComplex[A])(implicit r: Renderable[A]): StableEditor[A, F] = 
    new StableEditor(f)(SCardinal(c.map(Some(_))))

  // def apply[A, F <: ActiveFramework](f: F)(c: SComplex[Option[A]])(implicit r: Renderable[A]): StableEditor[A, F] = 
  //   new StableEditor(f)(SCardinal(c))

}
