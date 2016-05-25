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


  // This is the answer you should get for the complex.
  // ||(SBox(Some(+),SNode(SBox(Some(6),SNode(SBox(Some(2),SNode(SDot(Some(1)),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf)))) >> 
  // SBox(Some(+),SNode(SDot(Some(-)),SNode(SNode(SBox(Some(8),SNode(SDot(Some(7)),SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SNode(SBox(Some(4),SNode(SDot(Some(3)),SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SLeaf,SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf)))) >> 
  // SBox(Some(+),SNode(SDot(Some(_)),SNode(SLeaf,SNode(SNode(SNode(SDot(Some(9)),SNode(SLeaf,SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SNode(SNode(SDot(Some(5)),SNode(SLeaf,SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SLeaf,SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf))))) >> 
  // SDot(None)

  // This is what you *do* get
  // ||(SBox(+,SNode(SBox(None,SNode(SBox(Some(2),SNode(SDot(Some(1)),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf)))) >> 
  // SBox(+,SNode(SDot(-),SNode(SNode(SBox(None,SNode(SDot(None),SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SLeaf,SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf)))) >> 
  // SBox(+,SNode(SDot(-),SNode(SLeaf,SNode(SNode(SNode(SDot(None),SNode(SLeaf,SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SNode(SNode(SDot(Some(5)),SNode(SLeaf,SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SLeaf,SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf)))))


  // Dimension 1
  // SBox(Some(+),SNode(SDot(Some(-)),SNode(SNode(SBox(Some(8),SNode(SDot(Some(7)),SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SNode(SBox(Some(4),SNode(SDot(Some(3)),SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SLeaf,SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf)))) >> 
  // SBox(Some(+),SNode(SDot(Some(-)),SNode(SNode(SBox(Some(8),SNode(SDot(Some(7)),SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SLeaf,SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf)))) >> 

  // Dimension 2
  // SBox(Some(+),SNode(SDot(Some(_)),SNode(SLeaf,SNode(SNode(SNode(SDot(Some(9)),SNode(SLeaf,SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SNode(SNode(SDot(Some(5)),SNode(SLeaf,SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SLeaf,SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf))))) >> 
  // SBox(Some(+),SNode(SDot(Some(-)),SNode(SLeaf,SNode(SNode(SNode(SDot(Some(9)),SNode(SLeaf,SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SNode(SNode(SDot(Some(5)),SNode(SLeaf,SNode(SLeaf,SNode(SLeaf,SLeaf)))),SNode(SLeaf,SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf))),SNode(SLeaf,SLeaf)))))

  // Okay, so it's not the filler dimension that's the problem, it's actually the main extrusion dimension 
  // where you are losing some data.

}
