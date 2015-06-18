/**
  * NestingPanel.scala - Higher Dimensional Panel Implementation
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.newui

import opetopic._
import TypeDefs._

import syntax.tree._
import syntax.nesting._

abstract class NestingPanel[A, U : Numeric, P <: Nat] extends Panel[A, U, S[P]] {

  import isNumeric._

  //============================================================================================
  // EDGE NESTING GENERATION
  //

  def edgeNesting : Nesting[EdgeMarker, P] 

  def generateEdgeMarkers(p: P)(nst: Nesting[MarkerType, S[P]]) : Nesting[EdgeMarker, P] = {

    val edgeComp =
      nesting match {
        case Dot(_, sp) => fail("Base nesting is external!")
        case Box(_, cn) =>
          for {
            spine <- Nesting.spineFromCanopy(cn)
            res <- Tree.graftRec(p)(spine)(ad => {
              val em = new EdgeMarker
              succeed(Nesting.external(p)(em))
            })({ case (mk, cn) => {
              val em = new EdgeMarker
              mk.outgoingEdgeMarker = Some(em)
              succeed(Box(em, cn))
            }})
          } yield res
      }

    import scalaz.-\/
    import scalaz.\/-

    edgeComp match {
      case \/-(enst) => enst
      case _ => {
        // A dummy return, since this is an error ...
        Nesting.external(p)(new EdgeMarker)
      }
    }

  }

  def edgeLayoutTree : ShapeM[Tree[LayoutMarker, P]] = 
    for {
      spine <- Nesting.spineFromDerivative(edgeNesting, Zipper.globDerivative(edgeNesting.dim))
    } yield spine.map(em => {
      new LayoutMarker {

        val element = new EdgeStartRoot(em)
        val rootEdgeMarker = em
        val external = true

        override def leftInternalMargin = halfLeafWidth
        override def rightInternalMargin = halfLeafWidth

      }
    })

  //============================================================================================
  // EDGE ROOTS
  //

  class EdgeStartRoot(em : EdgeMarker) extends RootedElement {

    def rootX : U = em.edgeStartX
    def rootX_=(u : U) : Unit = 
      em.edgeStartX = u

    def rootY : U = em.edgeStartY
    def rootY_=(u : U) : Unit = 
      em.edgeStartY = u

  }

  class EdgeEndRoot(em : EdgeMarker) extends RootedElement {

    def rootX : U = em.edgeEndX
    def rootX_=(u : U) : Unit = 
      em.edgeEndX = u

    def rootY : U = em.edgeEndY
    def rootY_=(u : U) : Unit = 
      em.edgeEndY = u

  }

  //============================================================================================
  // LAYOUT NESTING
  //

  def layout : ShapeM[Unit] = 
    for {
      et <- edgeLayoutTree
      _ <- layoutNesting(nesting, et)
    } yield {

      // Well, you probably want to do some last minute sizing at the end here, no?

      ()

    }

  def layoutNesting[N <: Nat](nst : Nesting[RenderMarker, S[N]], lvs : Tree[LayoutMarker, N]) : ShapeM[LayoutMarker] = 
    nst match {
      case Dot(rm, d) => 
        for {
          outgoingEdge <- fromOpt(
            rm.outgoingEdgeMarker,
            new ShapeError("Missing outgoing edge for" ++ rm.toString)
          )
        } yield {

          rm.clear

          val newEdgeMarker = new EdgeStartRoot(outgoingEdge)

          rm.horizontalDependants += newEdgeMarker
          rm.verticalDependants += newEdgeMarker

          val leafMarkers = lvs.nodes
          val leafCount = leafMarkers.length

          // Zeroed out for external dots
          rm.leftInteriorMargin = zero
          rm.rightInteriorMargin = zero
          rm.interiorHeight = zero

          val marker =
            if (leafCount == 0) {  // This is a drop. Simply return an appropriate marker ...

              new LayoutMarker {

                val element = rm
                val external = false
                val rootEdgeMarker = outgoingEdge

                override def height = rm.height
                override def leftInternalMargin = rm.leftMargin
                override def rightInternalMargin = rm.rightMargin

              }

            } else { // We have children.  Arrange them and calculate the marker.

              val isOdd = (leafCount & 1) != 0

              val firstMarker = leafMarkers.head
              val lastMarker = leafMarkers.last

              val midMarker = leafMarkers(leafCount / 2)

              if (isOdd) {

                midMarker.rootEdgeMarker.edgeEndX = rm.rootX
                midMarker.rootEdgeMarker.edgeEndY = rm.rootY - rm.height

                val edgeMarker = new EdgeEndRoot(midMarker.rootEdgeMarker)

                rm.horizontalDependants += edgeMarker
                rm.verticalDependants += edgeMarker

                rm.horizontalDependants += midMarker.element

              }

              if (leafCount > 1) {

                val leftChildren = leafMarkers.slice(leafCount / 2 + (leafCount & 1), leafCount)
                val rightChildren = leafMarkers.slice(0, leafCount / 2)

                val leftChild = leftChildren.head
                val rightChild = rightChildren.last

                val midLeftOffset = if (isOdd) midMarker.leftMargin else zero
                val midRightOffset = if (isOdd) midMarker.rightMargin else zero

                val leftChildShift = max(midLeftOffset + externalPadding + leftChild.rightMargin, rm.leftMargin + externalPadding)
                val rightChildShift = max(midRightOffset + externalPadding + rightChild.leftMargin, rm.rightMargin + externalPadding)

                def doLeftPlacement(marker : LayoutMarker, shift : U) : Unit = {

                  marker.element.shiftLeft(shift)
                  rm.horizontalDependants += marker.element

                  val edgeMarker = new EdgeEndRoot(marker.rootEdgeMarker)

                  marker.rootEdgeMarker.edgeEndX = rm.x
                  marker.rootEdgeMarker.edgeEndY = rm.y + fullStrokeWidth + internalPadding + rm.halfLabelHeight

                  rm.horizontalDependants += edgeMarker
                  rm.verticalDependants += edgeMarker

                }

                def doRightPlacement(marker : LayoutMarker, shift : U) : Unit = {

                  marker.element.shiftRight(shift)
                  rm.horizontalDependants += marker.element

                  val edgeMarker = new EdgeEndRoot(marker.rootEdgeMarker)

                  marker.rootEdgeMarker.edgeEndX = rm.x + rm.width
                  marker.rootEdgeMarker.edgeEndY = rm.y + fullStrokeWidth + internalPadding + rm.halfLabelHeight

                  rm.horizontalDependants += edgeMarker
                  rm.verticalDependants += edgeMarker

                }

                doLeftPlacement(leftChild, leftChildShift)
                doRightPlacement(rightChild, rightChildShift)

                val leftEdge =
                  (leftChildren.tail foldLeft (leftChildShift + leftChild.leftMargin))({
                    case (leftShift, currentMarker) => {
                      val thisShift = leftShift + externalPadding + currentMarker.rightMargin
                      doLeftPlacement(currentMarker, thisShift)
                      thisShift + currentMarker.leftMargin
                    }
                  })

                val rightEdge =
                  (rightChildren.init foldRight (rightChildShift + rightChild.rightMargin))({
                    case (currentMarker, rightShift) => {
                      val thisShift = rightShift + externalPadding + currentMarker.leftMargin
                      doRightPlacement(currentMarker, thisShift)
                      thisShift + currentMarker.rightMargin
                    }
                  })

              }

              new LayoutMarker {

                val element = rm
                val external = false
                val rootEdgeMarker = outgoingEdge

                override def height = rm.height

                override def leftInternalMargin =
                  if (firstMarker.element.rootX < rm.x) {
                    (rm.rootX - firstMarker.element.rootX) + halfLeafWidth
                  } else {
                    rm.leftMargin
                  }

                override def rightInternalMargin =
                  if (lastMarker.element.rootX > (rm.x + rm.width)) {
                    (lastMarker.element.rootX - rm.rootX) + halfLeafWidth
                  } else {
                    rm.rightMargin
                  }

                override def leftSubtreeMargin =
                  if (firstMarker.element.rootX < rm.x) {
                    firstMarker.leftMargin - halfLeafWidth
                  } else zero

                override def rightSubtreeMargin =
                  if (lastMarker.element.rootX > (rm.x + rm.width)) {
                    lastMarker.rightMargin - halfLeafWidth
                  } else zero

              }

            }

          marker

        }

      case Box(rm, cn) => {

        rm.clear

        val (leafCount : Int, leavesWithIndices : Tree[(LayoutMarker, Int), N]) = lvs.zipWithIndex

        def verticalPass(tr : Tree[Nesting[RenderMarker, S[N]], S[N]]) : ShapeM[LayoutMarker] = 
          Tree.graftRec[Nesting[RenderMarker, S[N]], LayoutMarker, N](tr)({
            case addr => 
              for {
                leafMarkerWithIndex <- Tree.valueAt(leavesWithIndices, addr)
              } yield {

                val (leafMarker, leafIndex) = leafMarkerWithIndex

                if (leafIndex == 0 && leafCount == 1) {
                  leafMarker.truncateUnique
                } else if (leafIndex == 0) {
                  leafMarker.truncateRight
                } else if (leafIndex == leafCount - 1) {
                  leafMarker.truncateLeft
                } else {
                  leafMarker.truncateMiddle
                }
              }
          })({
            case (sn, layoutTree) => 
              for {
                localLayout <- layoutNesting(sn, layoutTree)
              } yield {

                val descendantMarkers : List[LayoutMarker] = layoutTree.nodes

                val (leftMostChild, rightMostChild, heightOfChildren) =
                  (descendantMarkers foldLeft (localLayout, localLayout, zero))({
                    case ((lcMarker, rcMarker, ht), thisMarker) => {

                      if (! thisMarker.external) {
                        thisMarker.element.shiftUp(localLayout.height + externalPadding)
                        localLayout.element.verticalDependants += thisMarker.element
                      }

                      val newLeftChild = if (thisMarker.leftEdge < lcMarker.leftEdge) thisMarker else lcMarker
                      val newRightChild = if (thisMarker.rightEdge > rcMarker.rightEdge) thisMarker else rcMarker

                      (newLeftChild, newRightChild, max(ht, thisMarker.height))

                    }
                  })

                val marker = new LayoutMarker {

                  val element = localLayout.element
                  val external = false
                  val rootEdgeMarker = localLayout.rootEdgeMarker

                  override def height = localLayout.height + externalPadding + heightOfChildren

                  override def leftInternalMargin =
                    (localLayout.element.rootX  - leftMostChild.element.rootX) + leftMostChild.leftInternalMargin

                  override def rightInternalMargin =
                    (rightMostChild.element.rootX - localLayout.element.rootX) + rightMostChild.rightInternalMargin

                  override def leftSubtreeMargin = leftMostChild.leftSubtreeMargin
                  override def rightSubtreeMargin = rightMostChild.rightSubtreeMargin

                }

                marker

              }
          })

        for {
          layout <- verticalPass(cn)
        } yield {

          // Set interior margins
          rm.leftInteriorMargin = layout.leftMargin
          rm.rightInteriorMargin = layout.rightMargin
          rm.interiorHeight = layout.height

          // Even if the layout was external  (meaning that we just
          // rendered a loop) we need to move it horizontally
          rm.horizontalDependants += layout.element

          if (! layout.external) {
            layout.element.shiftUp(fullStrokeWidth + rm.labelHeight + internalPadding + internalPadding)
            rm.verticalDependants += layout.element
          }

          // Setup and return an appropriate marker
          val marker = new LayoutMarker {

            val element = rm
            val external = false
            val rootEdgeMarker = layout.rootEdgeMarker

            override def height = rm.height
            override def leftInternalMargin = rm.leftMargin 
            override def rightInternalMargin = rm.rightMargin

          }

          marker

        }
      }
    }


}
