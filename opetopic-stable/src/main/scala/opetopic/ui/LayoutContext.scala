/**
  * LayoutContext.scala - Main Layout Routine
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.collection.mutable.Buffer

import opetopic._
import opetopic.mtl._

trait LayoutContext[F <: UIFramework] {

  val framework: F
  
  import framework._
  import isNumeric._

  type BoxType <: CellBox
  type EdgeType <: CellEdge

  //============================================================================================
  // LAYOUT PARAMETERS
  //

  def internalPadding: Size 
  def externalPadding: Size
  def leafWidth: Size
  def strokeWidth: Size
  def cornerRadius: Size
  
  def halfLeafWidth: Size = half(leafWidth)
  def halfStrokeWidth: Size = half(strokeWidth)

  //============================================================================================
  // CELL BOXES
  //

  trait CellBox extends Rooted { thisBox : BoxType =>

    def labelElement : Element 
    def labelBounds : Bounds 

    def isExternal: Boolean

    //
    // Mutable Values
    //

    var rootX: Size = zero
    var rootY: Size = zero

    var leftInteriorMargin: Size = zero
    var rightInteriorMargin: Size = zero

    var interiorHeight: Size = zero

    var outgoingEdge: Option[EdgeType] = None

    //
    // Derived Values
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
      horizontalDependents.clear
      verticalDependents.clear
    }

  }

  //============================================================================================
  // CELL EDGES
  //

  trait CellEdge { thisEdge : EdgeType =>

    //
    //  Mutable Values
    //

    var edgeStartX : Size = zero
    var edgeStartY : Size = zero

    var edgeEndX : Size = zero
    var edgeEndY : Size = zero

    //
    //  Path String Rendering
    //

    def pathString : String = {

      val isVertical : Boolean = edgeStartX == edgeEndX

      var pathString : String = "M " ++ edgeStartX.toString ++ " " ++ edgeStartY.toString ++ " "

      if (isVertical) {
        pathString ++= "V " ++ edgeEndY.toString
      } else {
        pathString ++= "V " ++ (edgeEndY - cornerRadius).toString ++ " "
        pathString ++= "A " ++ cornerRadius.toString ++ " " ++ cornerRadius.toString ++ " 0 0 " ++ (if (edgeStartX > edgeEndX) "1 " else "0 ") ++
          (if (edgeStartX > edgeEndX) (edgeStartX - cornerRadius) else (edgeStartX + cornerRadius)).toString ++ " " ++ edgeEndY.toString ++ " "
        pathString ++= "H " ++ edgeEndX.toString
      }

      pathString

    }

    def clearEdge: Unit = {
      edgeStartX = zero
      edgeStartY = zero
      edgeEndX = zero
      edgeEndY = zero
    }

  }

  //============================================================================================
  // MAIN LAYOUT ROUTINE
  //

  def layout(nst: SNesting[BoxType], lvs: STree[LayoutMarker]): Option[LayoutMarker] = 
    nst match {
      case SDot(bx) => {

        bx.clear

        val edgeMarker = 
          bx.outgoingEdge.map(edge => {
            edge.clearEdge
            EdgeStartMarker(edge)
          }).getOrElse(DummyMarker())

        bx.horizontalDependents += edgeMarker
        bx.verticalDependents += edgeMarker

        val leafMarkers = lvs.toList
        val leafCount = leafMarkers.length

        // Zeroed out for external dots
        bx.leftInteriorMargin = zero
        bx.rightInteriorMargin = zero
        bx.interiorHeight = zero


        val marker : LayoutMarker =
          if (leafCount == 0) {  // This is a drop. Simply return an appropriate marker ...

            LayoutMarker(
              bx, edgeMarker, false,
              height = bx.height,
              leftInternalMargin = bx.leftMargin,
              rightInternalMargin = bx.rightMargin
            )

          } else { // We have children.  Arrange them and calculate the marker.

            val isOdd = (leafCount & 1) != 0

            val firstMarker = leafMarkers.head
            val lastMarker = leafMarkers.last

            val midMarker = leafMarkers(leafCount / 2)

            if (isOdd) {

              val endMarker = midMarker.rootEdge.endMarker

              endMarker.rootX = bx.rootX
              endMarker.rootY = bx.rootY - bx.height

              bx.horizontalDependents += endMarker
              bx.verticalDependents += endMarker

              bx.horizontalDependents += midMarker.element

            }

            if (leafCount > 1) {

              val leftChildren = leafMarkers.slice(leafCount / 2 + (leafCount & 1), leafCount)
              val rightChildren = leafMarkers.slice(0, leafCount / 2)

              val leftChild = leftChildren.head
              val rightChild = rightChildren.last

              val midLeftOffset = if (isOdd) midMarker.leftMargin else zero
              val midRightOffset = if (isOdd) midMarker.rightMargin else zero

              val leftChildShift = max(midLeftOffset + externalPadding + leftChild.rightMargin, bx.leftMargin + externalPadding)
              val rightChildShift = max(midRightOffset + externalPadding + rightChild.leftMargin, bx.rightMargin + externalPadding)

              def doLeftPlacement(marker : LayoutMarker, shift : Size) : Unit = {

                marker.element.shiftLeft(shift)
                bx.horizontalDependents += marker.element

                val endMarker = marker.rootEdge.endMarker

                endMarker.rootX = bx.x
                endMarker.rootY = bx.y + strokeWidth + internalPadding + bx.halfLabelHeight

                bx.horizontalDependents += endMarker
                bx.verticalDependents += endMarker

              }

              def doRightPlacement(marker : LayoutMarker, shift : Size) : Unit = {

                marker.element.shiftRight(shift)
                bx.horizontalDependents += marker.element

                val endMarker = marker.rootEdge.endMarker

                endMarker.rootX = bx.x + bx.width
                endMarker.rootY = bx.y + strokeWidth + internalPadding + bx.halfLabelHeight

                bx.horizontalDependents += endMarker
                bx.verticalDependents += endMarker

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

              element = bx,
              rootEdge = edgeMarker,
              wasExternal = false,

              height = bx.height,

              leftInternalMargin =
                if (firstMarker.element.rootX < bx.x) {
                  (bx.rootX - firstMarker.element.rootX) + halfLeafWidth
                } else {
                  bx.leftMargin
                },

              rightInternalMargin =
                if (lastMarker.element.rootX > (bx.x + bx.width)) {
                  (lastMarker.element.rootX - bx.rootX) + halfLeafWidth
                } else {
                  bx.rightMargin
                },

              leftSubtreeMargin =
                if (firstMarker.element.rootX < bx.x) {
                  firstMarker.leftMargin - halfLeafWidth
                } else zero,

              rightSubtreeMargin =
                if (lastMarker.element.rootX > (bx.x + bx.width)) {
                  lastMarker.rightMargin - halfLeafWidth
                } else zero

            )

          }

        Some(marker)

      }
      case SBox(bx, cn) => {

        bx.clear

        val (leafCount : Int, leavesWithIndices : STree[(LayoutMarker, Int)]) =
          lvs.mapAccumL(0)((i: Int, m: LayoutMarker) => (i + 1, (m, i)))

        def verticalPass(tr : STree[SNesting[BoxType]]) : Option[LayoutMarker] =
          tr.treeFold[LayoutMarker]({
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
            case (snst, layoutTree) =>
              for {
                localLayout <- layout(snst, layoutTree)
              } yield {
                  
                // Zip together the incoming markers and any decoration information
                val descendantMarkers : List[(LayoutMarker, Option[BoundedElement])] =
                  layoutTree.map((_, None)).toList

                // ((for {
                //   lds <- leafDecorations
                //   ztr <- layoutTree.matchTraverse(lds)({
                //     case (l, o) => Some((l, o))
                //   })
                // } yield ztr) getOrElse layoutTree.map((_, None)).toList


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
          bx.leftInteriorMargin = layout.leftMargin
          bx.rightInteriorMargin = layout.rightMargin
          bx.interiorHeight = layout.height

          bx.horizontalDependents += layout.element

          if (! layout.wasExternal) {
            layout.element.shiftUp(strokeWidth + bx.labelHeight + internalPadding + internalPadding)
            bx.verticalDependents += layout.element
          }

          // Setup and return an appropriate marker
          val marker = LayoutMarker(
            element = bx,
            rootEdge = layout.rootEdge,
            wasExternal = false,
            height = bx.height,
            leftInternalMargin = bx.leftMargin,
            rightInternalMargin = bx.rightMargin
          )

          marker

        }
      }
    }

  //============================================================================================
  // ROOTED HELPER TRAIT
  //

  trait Rooted {

    var rootX : Size
    var rootY : Size

    val horizontalDependents : Buffer[Rooted] = Buffer.empty
    val verticalDependents : Buffer[Rooted] = Buffer.empty

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

  trait EdgeMarker extends Rooted {
    def endMarker: EdgeMarker
  }

  case class EdgeStartMarker(edge: EdgeType) extends EdgeMarker {

    def rootX : Size = edge.edgeStartX
    def rootX_=(u : Size) : Unit =
      edge.edgeStartX = u

    def rootY : Size = edge.edgeStartY
    def rootY_=(u : Size) : Unit =
      edge.edgeStartY = u

    def endMarker = EdgeEndMarker(edge)

  }

  case class EdgeEndMarker(edge : EdgeType) extends EdgeMarker {

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
