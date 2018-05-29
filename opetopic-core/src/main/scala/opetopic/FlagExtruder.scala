/**
  * FlagExtruder.scala - Extrude a complex from the list of its flags
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

object FlagExtruder {

  def extrudeFrom[A](itr: FlagIterator[A]): Unit = {

    // For now, since our iterators only work one way, we'll
    // just reverse the list, although this should be remedied
    // at some point.
    itr.toList.reverse match {
      case Nil => ()
      case hd::tl => {

        // We start with the first facet, which should be
        // a target if everything is set up correctly
        var last: Flag[A] = hd

        for {
          flg <- tl
        } {

          println("Passing flag: " ++ flg.map(_.toString).mkString(" "))

          sealed trait ExtrusionCmd
          case class Extrude(tgt: A, fill: A) extends ExtrusionCmd
          case class Loop(tgt: A, drop: A) extends ExtrusionCmd

          val suffix =
            (last, flg).zipped.dropWhile({
              case (f, g) => f.face == g.face
            }).unzip._2

          // Okay, now we have a suffix
          suffix match {
            case TgtFacet(tgt) :: TgtFacet(fill) :: _ => {
              println("Extrusion with (" ++ tgt.toString ++ ", " ++ fill.toString ++ ")")
            }
            case _ => ()
          }

          last = flg

        }


      }
    }

    


  }

}
