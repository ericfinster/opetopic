/**
  * CellBox.scala - Abstract Opetopic Cell Box
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.vf

import opetopic._
import TypeDefs._

trait CellBoxDefn[U] { vf: PanelDeps[U] =>

  import isFractional._

  object CellBox {

    @natElim
    def apply[A, N <: Nat](n: N)(
      panel: Panel[A, N],
      label: View[A],
      address: Address[S[N]],
      isExternal: Boolean
    ) : CellBox[A, N] = {
      case (Z, pnl, lbl, addr, ext) => new ZeroBox(pnl, lbl)
      case (S(p), pnl, lbl, addr, ext) => new SuccBox(pnl, lbl)
    }

  }

  // The box should already contain a component for the label ...
  abstract class CellBox[A, N <: Nat](panel: Panel[A, N], lbl: View[A]) extends Component with Rooted {

    import panel._

    val boxRect = Rectangle(x, y, width, height)

    def render: Seq[ElementType] = {
      lbl.component.render ++ boxRect.render
    }

    //
    // Mutable Values
    //

    var rootX : U = zero
    var rootY : U = zero

    var isExternal : Boolean = false

    var leftInteriorMargin : U = zero
    var rightInteriorMargin : U = zero

    var interiorHeight : U = zero

    // Now that this exists outside and with an index parameter, we should
    // be able to fix things up so that it's not an option in the correct dimensions ..
    var outgoingEdge : Option[CellEdge[A, N]] = None

    //
    // Derived Values
    //

    def x : U = rootX - leftMargin
    def y : U = rootY - height

    def interiorWidth : U = leftInteriorMargin + rightInteriorMargin

    def width : U = leftMargin + rightMargin
    def height : U =
      if (isExternal) {
        fullStrokeWidth +
        internalPadding +
        labelHeight +
        internalPadding +
        fullStrokeWidth
      } else {
        fullStrokeWidth +
        interiorHeight +
        internalPadding +
        labelHeight +
        internalPadding +
        fullStrokeWidth
      }

    def leftMargin : U =
      if (isExternal) {
        fullStrokeWidth + internalPadding + halfLabelWidth
      } else {
        fullStrokeWidth + leftInteriorMargin + internalPadding + fullStrokeWidth
      }

    def rightMargin : U =
      if (isExternal) {
        halfLabelWidth + internalPadding + fullStrokeWidth
      } else {
        max(
          internalPadding + labelWidth + internalPadding + fullStrokeWidth,
          rightInteriorMargin + internalPadding + fullStrokeWidth
        )
      }

    def halfLabelWidth : U = labelWidth / fromInt(2)
    def halfLabelHeight : U = labelHeight / fromInt(2)

    def labelWidth : U = lbl.width 
    def labelHeight : U = lbl.height 

    def clear : Unit = {
      rootX = zero
      rootY = zero
      leftInteriorMargin = zero
      rightInteriorMargin = zero
      interiorHeight = zero
      horizontalDependants.clear
      verticalDependants.clear
    }

  }

  class ZeroBox[A](panel: Panel[A, _0], lbl: View[A]) extends CellBox[A, _0](panel, lbl) 
  class SuccBox[A, P <: Nat](panel: Panel[A, S[P]], lbl: View[A]) extends CellBox[A, S[P]](panel, lbl)

}
