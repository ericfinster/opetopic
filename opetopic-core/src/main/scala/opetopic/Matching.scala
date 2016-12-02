/**
  * Matching.scala - Experiments with matching
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import mtl._

object Matching {

  def matchTest(c: SComplex[Unit]): Unit =
    c match {
      case tl >> SBox(_, SNode(a, s)) >> SBox(_, SNode(SDot(_), SNode(aa, ss))) >> SDot(_) => ()
      case tl >> SBox(_, s) >> SBox(_, SNode(SDot(_), ss)) >> SDot(_) => ()
      case tl >> SBox(_, s) >> SDot(_) => ()
      case tl >> SDot(_) => ()
      case tl >> hd => ()
      case ||(hd) => ()
    }

}
