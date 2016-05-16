/**
  * Experiment.scala - Experimenting with laziness
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

object Experiment {


  abstract class DataWrapper {
    val i: Int
    val j: Int
  }

  object DataWrapper {

    def apply(ii: => Int, jj: => Int): DataWrapper =
      new DataWrapper {
        lazy val i = ii
        lazy val j = jj
      }

  }

  val test: List[Int] = 
    List(1,2,3,4,5,6,7,8,9)

  def testLength: Int = {
    println("Calculating test length")
    test.length
  }

  def wrap: List[DataWrapper] = 
    test.map((i: Int) => 
      DataWrapper(i, testLength)
    )

  def doTest: Unit = 
    println(wrap.map(_.i.toString))

  import scalaz.Traverse
  import scalaz.syntax.traverse._

  val testTree: STree[Int] = 
    SNode(4, SNode(SNode(3, SLeaf), SLeaf))

  def mappedTree: STree[Int] = 
    testTree.map(_ + 1)

  def addrMappedTree: STree[Int] = 
    testTree.mapWithAddr((i, addr) => i + 1 )

  def derivMappedTree: STree[Int] = 
    testTree.mapWithDeriv[Unit, Int]((i, d) => { println(d.toString) ; i + 1 })

}
