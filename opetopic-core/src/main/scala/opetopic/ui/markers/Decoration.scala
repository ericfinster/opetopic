/**
  * Decoration.scala - EdgeDecorations
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui.markers

sealed trait TriangleDec {

  def next : TriangleDec = 
    this match {
      case Nonexistant => BlackTriangle
      case BlackTriangle => RedTriangle
      case RedTriangle => Nonexistant
    }

}

case object Nonexistant extends TriangleDec
case object BlackTriangle extends TriangleDec
case object RedTriangle extends TriangleDec
