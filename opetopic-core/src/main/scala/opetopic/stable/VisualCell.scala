/**
  * VisualCell.scala - A Cell with visualization attributes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.stable

import scala.collection.mutable.ListBuffer

import scalaz.Traverse
import scalaz.Applicative
import scalaz.syntax.traverse._
import scalaz.std.option._

import opetopic._
import opetopic.ui._

trait HasVisualCells { self : UIFramework =>

  import isNumeric._

  trait VisualCell[A, V <: VisualCell[A, V]] extends Cell[A, V] { thisCell : V =>

    //
    // Config Values

    def internalPadding : Size
    def externalPadding : Size
    def decorationPadding : Size
    def leafWidth : Size
    def strokeWidth : Size
    def cornerRadius : Size

    def halfLeafWidth: Size = half(leafWidth)

    //
    // Label Values and Elements
    //

    def labelElement: BoundedElement
    def labelBounds: Bounds =
      labelElement.bounds

    def rootDecoration: Option[BoundedElement] = None
    def leafDecorations: Option[STree[Option[BoundedElement]]] = None

    //
    // Mutable Cell Values
    //

    var rootX : Size = zero
    var rootY : Size = zero

    var leftInteriorMargin : Size = zero
    var rightInteriorMargin : Size = zero

    var interiorHeight : Size = zero

    //
    // Derived Cell Values
    //

    def x : Size = rootX - leftMargin
    def y : Size = rootY - height

    def interiorWidth : Size = leftInteriorMargin + rightInteriorMargin

    def width : Size = leftMargin + rightMargin
    def height : Size =
      if (isExternal) {
        strokeWidth +
        internalPadding +
        labelHeight +
        internalPadding +
        strokeWidth
      } else {
        strokeWidth +
        interiorHeight +
        internalPadding +
        labelHeight +
        internalPadding +
        strokeWidth
      }

    def leftMargin : Size =
      if (isExternal) {
        strokeWidth + internalPadding + halfLabelWidth
      } else {
        strokeWidth + leftInteriorMargin + internalPadding + strokeWidth
      }

    def rightMargin : Size =
      if (isExternal) {
        halfLabelWidth + internalPadding + strokeWidth
      } else {
        max(
          internalPadding + labelWidth + internalPadding + strokeWidth,
          rightInteriorMargin + internalPadding + strokeWidth
        )
      }

    def halfLabelWidth : Size = half(labelBounds.width)
    def halfLabelHeight : Size = half(labelBounds.height)

    def labelWidth : Size = labelBounds.width
    def labelHeight : Size = labelBounds.height

    def clear : Unit = {
      rootX = zero
      rootY = zero
      leftInteriorMargin = zero
      rightInteriorMargin = zero
      interiorHeight = zero
    }

    //
    //  Mutable Edge Values
    //

    var edgeStartX : Size = zero
    var edgeStartY : Size = zero

    var edgeEndX : Size = zero
    var edgeEndY : Size = zero
    
    //============================================================================================
    // LAYOUT ROUTINE
    //

    def layout(lvs: STree[LayoutMarker]) : Option[LayoutMarker] =
      canopy match {
        case None => {

          clear

          val boxMarker: Rooted = 
            BoxMarker[A, V](thisCell)

          val outgoingEdge: EdgeMarker = 
            target.map(EdgeStartMarker[A, V](_)) getOrElse DummyMarker()

          boxMarker.horizontalDependents += outgoingEdge
          boxMarker.verticalDependents += outgoingEdge

          val leafMarkers = lvs.toList
          val leafCount = leafMarkers.length

          // Zeroed out for external dots
          leftInteriorMargin = zero
          rightInteriorMargin = zero
          interiorHeight = zero

          val marker : LayoutMarker = 
            if (leafCount == 0) {  // This is a drop. Simply return an appropriate marker ...

              LayoutMarker(
                boxMarker, outgoingEdge, false,
                height = height,
                leftInternalMargin = leftMargin,
                rightInternalMargin = rightMargin
              )

            } else { // We have children.  Arrange them and calculate the marker.

              val isOdd = (leafCount & 1) != 0

              val firstMarker = leafMarkers.head
              val lastMarker = leafMarkers.last

              val midMarker = leafMarkers(leafCount / 2)

              if (isOdd) {

                midMarker.rootEdge.rootX = rootX
                midMarker.rootEdge.rootY = rootY - height

                val endMarker = midMarker.rootEdge.endMarker 

                boxMarker.horizontalDependents += endMarker
                boxMarker.verticalDependents += endMarker

                boxMarker.horizontalDependents += midMarker.element

              }

              if (leafCount > 1) {

                val leftChildren = leafMarkers.slice(leafCount / 2 + (leafCount & 1), leafCount)
                val rightChildren = leafMarkers.slice(0, leafCount / 2)

                val leftChild = leftChildren.head
                val rightChild = rightChildren.last

                val midLeftOffset = if (isOdd) midMarker.leftMargin else zero
                val midRightOffset = if (isOdd) midMarker.rightMargin else zero

                val leftChildShift = max(midLeftOffset + externalPadding + leftChild.rightMargin, leftMargin + externalPadding)
                val rightChildShift = max(midRightOffset + externalPadding + rightChild.leftMargin, rightMargin + externalPadding)

                def doLeftPlacement(marker : LayoutMarker, shift : Size) : Unit = {

                  marker.element.shiftLeft(shift)
                  boxMarker.horizontalDependents += marker.element

                  val endMarker = marker.rootEdge.endMarker

                  marker.rootEdge.rootX = x
                  marker.rootEdge.rootY = y + strokeWidth + internalPadding + halfLabelHeight

                  boxMarker.horizontalDependents += endMarker
                  boxMarker.verticalDependents += endMarker

                }

                def doRightPlacement(marker : LayoutMarker, shift : Size) : Unit = {

                  marker.element.shiftRight(shift)
                  boxMarker.horizontalDependents += marker.element

                  val endMarker = marker.rootEdge.endMarker

                  marker.rootEdge.rootX = x + width
                  marker.rootEdge.rootY = y + strokeWidth + internalPadding + halfLabelHeight

                  boxMarker.horizontalDependents += endMarker
                  boxMarker.verticalDependents += endMarker

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

              // 
              //  The finished marker for this external box
              //

              LayoutMarker(

                element = boxMarker,
                rootEdge = outgoingEdge,
                wasExternal = false,

                height = height,

                leftInternalMargin =
                  if (firstMarker.element.rootX < x) {
                    (rootX - firstMarker.element.rootX) + halfLeafWidth
                  } else {
                    leftMargin
                  },

                rightInternalMargin =
                  if (lastMarker.element.rootX > (x + width)) {
                    (lastMarker.element.rootX - rootX) + halfLeafWidth
                  } else {
                    rightMargin
                  },

                leftSubtreeMargin =
                  if (firstMarker.element.rootX < x) {
                    firstMarker.leftMargin - halfLeafWidth
                  } else zero,

                rightSubtreeMargin =
                  if (lastMarker.element.rootX > (x + width)) {
                    lastMarker.rightMargin - halfLeafWidth
                  } else zero

              )

            }

          Some(marker)

        }
        case Some(cn) => {

          clear

          val boxMarker: Rooted =
            BoxMarker[A, V](thisCell)

          val (leafCount : Int, leavesWithIndices : STree[(LayoutMarker, Int)]) =
            lvs.mapAccumL(0)((i: Int, m: LayoutMarker) => (i + 1, (m, i)))

          def verticalPass(tr : STree[V]) : Option[LayoutMarker] =
            tr.graftRec[LayoutMarker]({
              case addr => 
                for {
                  leafMarkerWithIndex <- leavesWithIndices.elementAt(addr) 
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
                  localLayout <- sn.layout(layoutTree)
                } yield {
                  
                  // Zip together the incoming markers and any decoration information
                  val descendantMarkers : List[(LayoutMarker, Option[BoundedElement])] =
                    ((for {
                      lds <- leafDecorations
                      ztr <- layoutTree.matchTraverse(lds)({
                        case (l, o) => Some((l, o))
                      })
                    } yield ztr) getOrElse (layoutTree map (l => (l, None)))).toList


                  val (leftMostChild, rightMostChild, heightOfChildren, topShim) =
                    (descendantMarkers foldLeft (localLayout, localLayout, zero, externalPadding))({
                      case ((lcMarker, rcMarker, ht, ts), (thisMarker, thisDecOpt)) => {

                        val thisShim = externalPadding
                          // thisDecOpt match {
                          //   case None => externalPadding
                          //   case Some(be) => {

                          //     val re = thisMarker.rootEdge
                          //     val decMkr = new re.DecorationMarker(be, re.edgeStartX, - localLayout.height - decorationPadding)
                          //     localLayout.element.horizontalDependants += decMkr
                          //     localLayout.element.verticalDependants += decMkr

                          //     be.bounds.height + decorationPadding + strokeWidth

                          //   }
                          // }

                        if (! thisMarker.wasExternal) {
                          thisMarker.element.shiftUp(localLayout.height + thisShim)
                          localLayout.element.verticalDependents += thisMarker.element
                        }

                        val newLeftChild = if (thisMarker.leftEdge < lcMarker.leftEdge) thisMarker else lcMarker
                        val newRightChild = if (thisMarker.rightEdge > rcMarker.rightEdge) thisMarker else rcMarker

                        (newLeftChild, newRightChild, max(ht, thisMarker.height), max(ts, thisShim))

                      }
                    })

                  // Calculate the bottom shim necessary to clear any outgoing edge marker
                  val bottomShim = zero
                    // localVis.rootEdgeElement match {
                    //   case None => zero
                    //   case Some(be) => {

                    //     val re = localLayout.rootEdge
                    //     val decMkr = new re.DecorationMarker(be, re.edgeStartX, decorationPadding)
                    //     localLayout.element.horizontalDependants += decMkr
                    //     localLayout.element.verticalDependants += decMkr

                    //     be.bounds.height + decorationPadding

                    //   }
                    // }

                  // Shift up the local layout to make room
                  localLayout.element.shiftUp(bottomShim)

                  LayoutMarker(
                    element = localLayout.element,
                    rootEdge = localLayout.rootEdge,
                    wasExternal = false,
                    height = bottomShim + localLayout.height + topShim + heightOfChildren,
                    leftInternalMargin = (localLayout.element.rootX  - leftMostChild.element.rootX) + leftMostChild.leftInternalMargin,
                    rightInternalMargin = (rightMostChild.element.rootX - localLayout.element.rootX) + rightMostChild.rightInternalMargin,
                    leftSubtreeMargin = leftMostChild.leftSubtreeMargin,
                    rightSubtreeMargin = rightMostChild.rightSubtreeMargin
                  )

                }
            })

          for {
            layout <- verticalPass(cn)
          } yield {

            // Set interior margins
            leftInteriorMargin = layout.leftMargin
            rightInteriorMargin = layout.rightMargin
            interiorHeight = layout.height

            boxMarker.horizontalDependents += layout.element

            if (! layout.wasExternal) {
              layout.element.shiftUp(strokeWidth + labelHeight + internalPadding + internalPadding)
              boxMarker.verticalDependents += layout.element
            }

            // Setup and return an appropriate marker
            val marker = LayoutMarker(
              element = boxMarker,
              rootEdge = layout.rootEdge,
              wasExternal = false,
              height = height,
              leftInternalMargin = leftMargin,
              rightInternalMargin = rightMargin
            )

            marker

          }
        }
      }


  }

  //============================================================================================
  // HELPER CLASSES
  //

  trait Rooted {

    var rootX : Size
    var rootY : Size

    val horizontalDependents : ListBuffer[Rooted] = ListBuffer.empty
    val verticalDependents : ListBuffer[Rooted] = ListBuffer.empty

    def shiftRight(amount : Size) : Unit = {
      if (amount != 0) {
        rootX = (rootX + amount)
        horizontalDependents foreach (_.shiftRight(amount))
      }
    }

    def shiftDown(amount : Size) : Unit = {
      if (amount != 0) {
        rootY = (rootY + amount)
        verticalDependents foreach (_.shiftDown(amount))
      }
    }

    def shiftLeft(amount : Size) : Unit = shiftRight(-amount)
    def shiftUp(amount : Size) : Unit = shiftDown(-amount)

  }

  case class LayoutMarker(
    val element: Rooted,
    val rootEdge: EdgeMarker,
    val wasExternal: Boolean,
    val height: Size = zero,
    val leftSubtreeMargin: Size = zero,
    val rightSubtreeMargin: Size = zero,
    val leftInternalMargin: Size = zero,
    val rightInternalMargin: Size = zero
  ) {

    def leftMargin: Size = leftSubtreeMargin + leftInternalMargin
    def rightMargin: Size = rightSubtreeMargin + rightInternalMargin

    def leftEdge: Size = element.rootX - leftMargin
    def rightEdge: Size = element.rootX + rightMargin

    // Truncations

    def truncateLeft: LayoutMarker =
      LayoutMarker(
        element, rootEdge, true,
        rightSubtreeMargin = rightSubtreeMargin,
        rightInternalMargin = rightInternalMargin
      )

    def truncateRight: LayoutMarker =
      LayoutMarker(
        element, rootEdge, true,
        leftSubtreeMargin = leftSubtreeMargin,
        leftInternalMargin = leftInternalMargin
      )

    def truncateUnique: LayoutMarker =
      LayoutMarker(element, rootEdge, true)

    def truncateMiddle : LayoutMarker =
      LayoutMarker(
        element, rootEdge, true,
        leftSubtreeMargin = leftSubtreeMargin,
        rightSubtreeMargin = rightSubtreeMargin,
        leftInternalMargin = leftInternalMargin,
        rightInternalMargin = rightInternalMargin
      )

    override def toString = "LM(" + element.toString + ")" +
    "(we = " + wasExternal.toString + ", ht = " + height.toString +
    ", re = " + rootEdge.toString +
    ", rx = " + element.rootX.toString +
    ", ry = " + element.rootY.toString +
    ", lsm = " + leftSubtreeMargin.toString +
    ", lim = " + leftInternalMargin.toString +
    ", rim = " + rightInternalMargin.toString +
    ", rsm = " + rightSubtreeMargin.toString + ")"

  }

  case class BoxMarker[A, V <: VisualCell[A, V]](box: V) extends Rooted {

    def rootX : Size = box.rootX
    def rootX_=(u : Size) : Unit =
      box.rootX = u

    def rootY : Size = box.rootY
    def rootY_=(u : Size) : Unit =
      box.rootY = u

  }

  trait EdgeMarker extends Rooted {
    def endMarker: EdgeMarker
  }

  case class EdgeStartMarker[A, V <: VisualCell[A, V]](edge: V) extends EdgeMarker {

    def rootX : Size = edge.edgeStartX
    def rootX_=(u : Size) : Unit =
      edge.edgeStartX = u

    def rootY : Size = edge.edgeStartY
    def rootY_=(u : Size) : Unit =
      edge.edgeStartY = u

    def endMarker = EdgeEndMarker[A, V](edge)

  }

  case class EdgeEndMarker[A, V <: VisualCell[A, V]](edge : V) extends EdgeMarker {

    def rootX : Size = edge.edgeEndX
    def rootX_=(u : Size) : Unit =
      edge.edgeEndX = u

    def rootY : Size = edge.edgeEndY
    def rootY_=(u : Size) : Unit =
      edge.edgeEndY = u

    def endMarker = this

  }

  case class DummyMarker() extends EdgeMarker {
    var rootX: Size = zero
    var rootY: Size = zero
    def endMarker = this
  }


}
