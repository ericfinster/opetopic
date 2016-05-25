/**
  * Experiment.scala - Experimenting with laziness
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

object Experiment {

  import opetopic._
  import opetopic.Examples._

  val fredSComplex: SComplex[Int] = 
    SComplex(fredComplex)

  val arrow: SComplex[Int] = 
    ||(SBox(1, SNode(SDot(2), SNode(SLeaf, SLeaf)))) >> SDot(3)

  val simplex: SComplex[Int] = 
    ||(SBox(1, STree.obj(SBox(2, STree.obj(SDot(3)))))) >>
      SBox(4, STree.lst(List(SDot(5), SDot(6)))) >>
      SDot(7)


}
