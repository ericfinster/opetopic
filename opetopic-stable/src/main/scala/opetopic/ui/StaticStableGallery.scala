/**
  * StaticStableGallery.scala - A Static Gallery for static rendering
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

abstract class StaticStableGallery[F <: UIFramework](frmwk: F) 
    extends StableGallery[F](frmwk) {

  import framework._
  import isNumeric._

  type BoxType <: StaticBox
  type EdgeType <: StaticEdge

  type PanelType <: StaticPanel

  trait StaticBox extends GalleryBox { thisBox : BoxType => }
  trait StaticEdge extends CellEdge { thisEdge : EdgeType => }

  trait StaticPanel extends StablePanel { thisPanel : PanelType => }


}


