/**
  * SelectableGallery.scala - Galleries which keep a collection of selected cells
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.collection.mutable.ListBuffer

import opetopic._
import syntax.suite._
import syntax.nesting._

trait HasSelectablePanels extends HasPanels { self: UIFramework =>

  trait SelectablePanel[A, N <: Nat] { self : Panel[A, N] =>

    type BoxType <: SelectableBox[A, N] { type BoxAddressType = PanelAddressType }

  }

  trait SelectableBox[A, N <: Nat] extends CellBox[A, N] {

    def canSelect: Boolean = true
    var isSelected: Boolean = false

    def select: Unit = ()
    def deselect: Unit = ()

  }


}


trait HasSelectableGalleries extends HasGalleries { self: UIFramework with HasSelectablePanels =>

  trait SelectableGallery[A[_ <: Nat]] extends Gallery[A] {

    type PanelType[N <: Nat] <: SelectablePanel[A[N], N] with GalleryPanel[N]
    type GalleryBoxType[N <: Nat] <: SelectableBox[A[N], N] { type BoxAddressType = GalleryAddressType[N] }

    var selection: Option[Selection]

    trait Selection {

      type Dim <: Nat

      val dim: Dim
      val root: GalleryBoxType[Dim]
      val companions: ListBuffer[GalleryBoxType[Dim]]

    }

    object Selection {

      def apply[N <: Nat](box: GalleryBoxType[N]) : Selection =
        new Selection {
          type Dim = N
          val dim = box.boxDim
          val root = box
          val companions = ListBuffer[GalleryBoxType[Dim]]()
        }

    }

    def deselectAll : Unit = {
      for {
        sel <- selection
        _ = sel.root.deselect
        box <- sel.companions
      } {
        box.deselect
      }

      selection = None
    }
    
    def selectAsRoot[N <: Nat](box : GalleryBoxType[N]) : Unit =
      if (box.canSelect) {
        deselectAll
        box.select
        //onSelectAsRoot(marker.dim)(marker)
        selection = Some(Selection(box))
      }

    // for {
    //   zipper <- panel.nesting.seekTo(nestingAddress)
    // } yield {
    //   println("Complex address gives cell: " ++ zipper._1.baseValue.toString)
    // }

    def select[N <: Nat](box: GalleryBoxType[N]) : Unit = 
      if (box.canSelect) {
        selection match {
          case None => selectAsRoot(box)
          case Some(sel) => {

            import TypeLemmas._

            matchNatPair(box.boxDim, sel.dim) match {
              case None => selectAsRoot(box)
              case Some(ev) => {

                val candidates: ListBuffer[GalleryBoxType[N]] = 
                  ListBuffer()

                val thePanels = panels

                for {
                  diff <- fromOpt(diffOpt(box.boxDim, thePanels.value.length.pred))
                  thePanel = thePanels.get(box.boxDim)(diff)
                  zipper <- thePanel.boxNesting.seekTo(box.nestingAddress)
                } yield {

                  import scalaz.-\/
                  import scalaz.\/-

                  Nesting.predecessorWhich(zipper)(b => {
                    candidates += b
                    b.isSelected
                  }) match {
                    case -\/(_) => selectAsRoot(box)
                    case \/-((fcs, _)) => {
                      for {
                        b <- candidates.init
                      } {
                        b.select
                        sel.companions += rewriteNatIn(ev)(b)
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }

  }

}
