/**
  * StaticMultiCardinalGallery.scala - A static gallery for rendering multi-cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

sealed trait MultiCard[+A]
case class Base[+A](c: SCardinal[A]) extends MultiCard[A]
case class Embed[+A](mc: SCardinal[MultiCard[A]]) extends MultiCard[A]

class StaticMultiCardinalGallery[A, F <: UIFramework](frmwk: F)(val mc: MultiCard[A])(implicit rn: Renderable[A, F]) {


  // class LevelGallery extends StaticStableGallery(frmwk) with CardinalGallery[A]  {
  // }

}
