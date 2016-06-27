/**
  * ComplexGallery.scala - A Trait for a gallery containing a complex
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import opetopic._
import opetopic.mtl._

trait ComplexGallery[F <: UIFramework] { thisGallery: StableGallery[F] => 

  type PanelType <: ComplexPanel

  trait ComplexPanel { thisPanel : PanelType => 

    val edgeData: Either[PanelType, SNesting[EdgeType]]

    def edgeNesting: SNesting[CellType] = 
      edgeData match {
        case Left(pp) => pp.boxNesting
        case Right(en) => en
      }

    def refreshEdges: Unit = 
      edgeData match {
        case Left(pp) => {

          boxNesting match {
            case SDot(c) => c.outgoingEdge = Some(pp.boxNesting.baseValue)
            case SBox(_, cn) => 
              for {
                sp <- cn.spine
                _ <- sp.matchTraverse[EdgeType, Unit](pp.boxNesting.toTree)({
                  case (c, e) => Some({ c.outgoingEdge = Some(e) })
                })
              } { }
          }
          
        }
        case Right(en) => {
          boxNesting.map(c => c.outgoingEdge = Some(en.baseValue))
        }
      }

    // Refresh upon initialization
    refreshEdges

  }

  def createCell(lbl: LabelType, dim: Int, addr: SAddr, isExternal: Boolean): CellType
  def createPanel(bn: SNesting[CellType], en: Either[PanelType, SNesting[CellType]]): PanelType

  def buildCells(dim: Int, n: SNesting[LabelType]): SNesting[CellType] =
    n.foldNestingWithAddr[SNesting[BoxType]]()({
      case (a, addr) => SDot(createCell(a, dim, addr, true))
    })({
      case (a, addr, cn) => SBox(createCell(a, dim, addr, false), cn)
    })

  def buildPanels(c: SComplex[LabelType]) : Suite[PanelType] =
    c match {
      case ||(n) => {

        val objNesting = buildCells(0, n)
        val inEdge = createCell(n.baseValue, -1, Nil, true)
        val outEdge = createCell(n.baseValue, -1, Nil, false)
        val panel = createPanel(objNesting, Right(SBox(outEdge, STree.obj(SDot(inEdge)))))

        ||(panel)

      }
      case tl >> hd => {

        val tailPanels = buildPanels(tl)
        val prevPanel = tailPanels.head

        val panelCells = buildCells(prevPanel.dim + 1, hd)
        val panel = createPanel(panelCells, Left(prevPanel))

        tailPanels >> panel

      }
    }


}
