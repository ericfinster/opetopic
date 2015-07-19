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

trait HasSelectablePanels extends HasPanels { self: UIFramework =>

  trait SelectablePanel[A, E <: Element, N <: Nat] { thisPanel: Panel[A, E, N] =>

    type BoxType <: SelectableBox

    trait SelectableBox extends CellBox {

      def canSelect: Boolean = true
      var isSelected: Boolean = false

      def select: Unit = ()
      def deselect: Unit = ()

    }

  }

}


trait HasSelectableGalleries extends HasGalleries { self: UIFramework with HasSelectablePanels =>

  trait SelectableGallery[A[_ <: Nat], E <: Element] extends Gallery[A, E] {

    type PanelType[N <: Nat] <: Panel[A[N], E, N] with SelectablePanel[A[N], E, N] with GalleryPanel[N]

    var selection: Option[Selection]

    trait Selection {

      type Dim <: Nat

      val dim: Dim
      val root: PanelBoxType[Dim]
      val companions: ListBuffer[PanelBoxType[Dim]]

    }

    object Selection {

      def apply[N <: Nat](box: PanelBoxType[N]) : Selection =
        new Selection {
          type Dim = N
          val dim = box.boxDim
          val root = box
          val companions = ListBuffer[PanelBoxType[Dim]]()
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
    
    def selectAsRoot[N <: Nat](box : PanelBoxType[N]) : Unit =
      if (box.canSelect) {
        deselectAll
        box.select
        //onSelectAsRoot(marker.dim)(marker)
        selection = Some(Selection(box))
      }


    def select[N <: Nat](box: PanelBoxType[N]) : Unit = 
      if (box.canSelect) {
        selection match {
          case None => selectAsRoot(box)
          case Some(sel) => {

            import TypeLemmas._

            matchNatPair(box.boxDim, sel.dim) match {
              case None => selectAsRoot(box)
              case Some(ev) => {

                val candidates: ListBuffer[PanelBoxType[N]] = 
                  ListBuffer()

                val thePanels = panels

                for {
                  diff <- fromOpt(diffOpt(box.boxDim, thePanels.value.length.pred))
                  thePanel = thePanels.get(box.boxDim)(diff)
                  zipper <- thePanel.seekToAddress(box.address)
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
