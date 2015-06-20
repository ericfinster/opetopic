/**
  * CellEdge.scala - Opetopic Cell Edge
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.vf

import opetopic._
import TypeDefs._

trait CellEdgeDefn[U] { vf: PanelDeps[U] =>

  import isFractional._

  def cellEdge[A, N <: Nat](n: N)(
    panel: Panel[A, N]
  ) : CellEdge[A, N] = ???

  trait EdgeLike {

    var edgeStartX : U = zero
    var edgeStartY : U = zero

    var edgeEndX : U = zero
    var edgeEndY : U = zero

  }

  abstract class CellEdge[A, N <: Nat](panel: Panel[A, N]) extends Component with EdgeLike {

  }

}
