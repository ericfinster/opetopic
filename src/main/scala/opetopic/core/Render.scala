/**
  * Render.scala - Rendering Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds

import scalaz.syntax.monad._

trait Renderer[M[+_], U]  {

  implicit val isShapeMonad : ShapeMonad[M]
  import isShapeMonad._

  implicit val isNumeric : Numeric[U]
  import isNumeric._

  //============================================================================================
  // RENDERING OPTIONS
  //

  def halfLeafWidth : U
  def halfStrokeWidth : U

  def strokeWidth = fromInt(2) * halfStrokeWidth
  def leafWidth = fromInt(2) * halfLeafWidth

  def internalPadding : U
  def externalPadding : U

  //============================================================================================
  // ROOTED ELEMENTS
  //

  trait RootedElement {

    var rootX : U 
    var rootY : U 

    var horizontalDependants : List[RootedElement] = Nil
    var verticalDependants : List[RootedElement] = Nil

    def shiftRight(amount : U) : Unit = {
      if (amount != 0) {
        rootX = (rootX + amount)
        horizontalDependants foreach (_.shiftRight(amount))
      }
    }

    def shiftLeft(amount : U) : Unit = {
      if (amount != 0) {
        rootX = (rootX - amount)
        horizontalDependants foreach (_.shiftLeft(amount))
      }
    }

    def shiftDown(amount : U) : Unit = {
      if (amount != 0) {
        rootY = (rootY + amount)
        verticalDependants foreach (_.shiftDown(amount))
      }
    }

    def shiftUp(amount : U) : Unit = {
      if (amount != 0) {
        rootY = (rootY - amount)
        verticalDependants foreach (_.shiftUp(amount))
      }
    }

  }

  class EdgeStartMarker(rm : RenderMarker) extends RootedElement {

    def rootX : U = rm.edgeStartX
    def rootX_=(u : U) : Unit = 
      rm.edgeStartX = u

    def rootY : U = rm.edgeStartY
    def rootY_=(u : U) : Unit = 
      rm.edgeStartY = u

    override def toString = "Edge(" ++ rm.toString ++ ")"

  }

  class EdgeEndMarker(rm : RenderMarker) extends RootedElement {

    def rootX : U = rm.edgeEndX
    def rootX_=(u : U) : Unit = 
      rm.edgeEndX = u

    def rootY : U = rm.edgeEndY
    def rootY_=(u : U) : Unit = 
      rm.edgeEndY = u

  }

  //============================================================================================
  // RENDER MARKER 
  //

  trait RenderMarker extends RootedElement {

    def isExternal : Boolean 

    // Box Data

    def x : U = rootX - leftMargin
    def y : U = rootY - height

    def width : U = leftMargin + rightMargin 
    def height : U =
      if (isExternal) {
        strokeWidth + 
        internalPadding + 
        labelHeight + 
        internalPadding + 
        strokeWidth
      } else {
        strokeWidth +
        labelContainerHeight +
        interiorHeight +
        internalPadding +
        strokeWidth
      }

    def leftMargin : U =
      if (isExternal) {
        halfLabelWidth + internalPadding + strokeWidth
      } else {
        strokeWidth + leftInteriorMargin + internalPadding + strokeWidth
      }

    def rightMargin : U =
      if (isExternal) {
        halfLabelWidth + internalPadding + strokeWidth
      } else {
        max(
          labelContainerWidth + strokeWidth,
          strokeWidth + rightInteriorMargin + internalPadding + strokeWidth
        )
      }

    def halfLabelWidth : U
    def halfLabelHeight : U

    def labelWidth : U = halfLabelWidth * fromInt(2)
    def labelHeight : U = halfLabelHeight * fromInt(2)

    def labelContainerWidth : U = strokeWidth + labelWidth + internalPadding
    def labelContainerHeight : U = internalPadding + labelHeight + internalPadding

    var leftInteriorMargin : U = zero
    var rightInteriorMargin : U = zero

    def interiorWidth : U = leftInteriorMargin + rightInteriorMargin 
    var interiorHeight : U = zero

    // Edge Data

    var edgeStartX : U = zero
    var edgeStartY : U = zero

    var edgeEndX : U = zero
    var edgeEndY : U = zero

    def outgoingEdgeMarker : M[RenderMarker]

    def clear : Unit = {
      rootX = zero
      rootY = zero
      leftInteriorMargin = zero
      rightInteriorMargin = zero 
      interiorHeight = zero
      edgeStartX = zero
      edgeStartY = zero
      edgeEndX = zero
      edgeEndY = zero
      horizontalDependants = Nil
      verticalDependants = Nil
    }

  }

  //============================================================================================
  // LAYOUT MARKER
  //

  abstract class LayoutMarker { thisMarker =>

    val element : RootedElement
    val external : Boolean

    val rootEdgeMarker : RenderMarker

    def height : U = zero

    def leftSubtreeMargin : U = zero
    def rightSubtreeMargin : U = zero
    def leftInternalMargin : U = zero
    def rightInternalMargin : U = zero

    def leftMargin : U = leftSubtreeMargin + leftInternalMargin 
    def rightMargin : U = rightSubtreeMargin + rightInternalMargin

    def leftEdge : U = element.rootX - leftMargin
    def rightEdge : U = element.rootX + rightMargin

    // Truncations

    def truncateLeft : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val external = true
        val rootEdgeMarker = thisMarker.rootEdgeMarker
        override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
        override def rightInternalMargin = thisMarker.rightInternalMargin
      }

    def truncateRight : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val external = true
        val rootEdgeMarker = thisMarker.rootEdgeMarker
        override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
        override def leftInternalMargin = thisMarker.leftInternalMargin
      }

    def truncateUnique : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val rootEdgeMarker = thisMarker.rootEdgeMarker
        val external = true
      }

    def truncateMiddle : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val external = true
        val rootEdgeMarker = thisMarker.rootEdgeMarker
        override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
        override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
        override def leftInternalMargin = thisMarker.leftInternalMargin
        override def rightInternalMargin = thisMarker.rightInternalMargin
      }

    override def toString = "LM(" ++ element.toString ++ ")" ++ 
      "(we = " ++ external.toString ++ ", ht = " ++ height.toString ++
      ", re = " ++ rootEdgeMarker.toString ++
      ", rx = " ++ element.rootX.toString ++
      ", ry = " ++ element.rootY.toString ++
      ", lsm = " ++ leftSubtreeMargin.toString ++
      ", lim = " ++ leftInternalMargin.toString ++
      ", rim = " ++ rightInternalMargin.toString ++
      ", rsm = " ++ rightSubtreeMargin.toString ++ ")"

  }

  //============================================================================================
  // RENDER OBJECT NESTING
  //

  def renderObjectNesting(nst : Nesting[RenderMarker, _0]) : RenderMarker = 
    nst match {
      case Obj(rm) => {
        rm.clear
        rm
      }
      case Box(rm, Pt(n)) => {

        val internalMarker = renderObjectNesting(n)

        rm.clear

        rm.leftInteriorMargin = internalMarker.leftMargin
        rm.rightInteriorMargin = internalMarker.rightMargin
        rm.interiorHeight = internalMarker.height

        internalMarker.shiftUp(strokeWidth + rm.labelContainerHeight)

        rm.horizontalDependants :+= internalMarker
        rm.verticalDependants :+= internalMarker

        rm
      }
    }

  //============================================================================================
  // RENDER NESTING
  //

  def renderNesting[N <: Nat](nst : Nesting[RenderMarker, S[N]], lvs : Tree[LayoutMarker, N]) : M[LayoutMarker] = 
    nst match {
      case Dot(rm, d) => 
        for {
          outgoingEdge <- rm.outgoingEdgeMarker
        } yield {

          rm.clear

          val newEdgeMarker = new EdgeStartMarker(outgoingEdge)

          rm.horizontalDependants :+= newEdgeMarker
          rm.verticalDependants :+= newEdgeMarker

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

                // println("Mid marker is: " ++ midMarker.toString)

                // println("Dot coords: (" ++ rm.rootX.toString ++ ", " ++ rm.rootY.toString ++ ", " ++
                //   rm.width.toString ++ ", " ++ rm.height.toString ++ ")")

                midMarker.rootEdgeMarker.edgeEndX = rm.rootX
                midMarker.rootEdgeMarker.edgeEndY = rm.rootY - rm.height

                val edgeMarker = new EdgeEndMarker(midMarker.rootEdgeMarker)

                rm.horizontalDependants :+= edgeMarker
                rm.verticalDependants :+= edgeMarker

                rm.horizontalDependants :+= midMarker.element

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

                  // println("Placing marker: " ++ marker.toString)
                  // println("Shifting " ++ marker.toString ++ " left by " ++ shift.toString)

                  marker.element.shiftLeft(shift)
                  rm.horizontalDependants :+= marker.element

                  val edgeMarker = new EdgeEndMarker(marker.rootEdgeMarker)

                  marker.rootEdgeMarker.edgeEndX = rm.x
                  marker.rootEdgeMarker.edgeEndY = rm.y + strokeWidth + internalPadding + rm.halfLabelHeight

                  rm.horizontalDependants :+= edgeMarker
                  rm.verticalDependants :+= edgeMarker

                }

                def doRightPlacement(marker : LayoutMarker, shift : U) : Unit = {

                  // println("Passing marker: " ++ marker.toString)
                  // println("Shifting " ++ marker.toString ++ " right by " ++ shift.toString)

                  marker.element.shiftRight(shift)
                  rm.horizontalDependants :+= marker.element

                  val edgeMarker = new EdgeEndMarker(marker.rootEdgeMarker)

                  marker.rootEdgeMarker.edgeEndX = rm.x + rm.width
                  marker.rootEdgeMarker.edgeEndY = rm.y + strokeWidth + internalPadding + rm.halfLabelHeight

                  rm.horizontalDependants :+= edgeMarker
                  rm.verticalDependants :+= edgeMarker

                }

                // println("Left child shift: " ++ leftChildShift.toString)
                // println("Right child shift: " ++ rightChildShift.toString)

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

          // println("Rendered dot: " ++ rm.toString)

          marker
        }

      case Box(rm, cn) => {

        rm.clear

        val (leafCount : Int, leavesWithIndices : Tree[(LayoutMarker, Int), N]) = lvs.zipWithIndex

        def verticalPass(tr : Tree[Nesting[RenderMarker, S[N]], S[N]]) : M[LayoutMarker] = 
          Tree.graftRec[M, Nesting[RenderMarker, S[N]], LayoutMarker, N](tr)({
            case addr => 
              for {
                leafMarkerWithIndex <- Tree.valueAt(leavesWithIndices, addr)
              } yield {

                val (leafMarker, leafIndex) = leafMarkerWithIndex

                if (leafIndex == 0 && leafCount == 1) {
                  leafMarker.truncateUnique
                } else if (leafIndex == 0) {
                  // println("Truncating right: " ++ leafMarker.toString)
                  leafMarker.truncateRight
                } else if (leafIndex == leafCount - 1) {
                  // println("Truncating left: " ++ leafMarker.toString)
                  leafMarker.truncateLeft
                } else {
                  leafMarker.truncateMiddle
                }
              }
          })({
            case (sn, layoutTree) => 
              for {
                localLayout <- renderNesting(sn, layoutTree)
              } yield {

                val descendantMarkers : List[LayoutMarker] = layoutTree.nodes

                val (leftMostChild, rightMostChild, heightOfChildren) =
                  (descendantMarkers foldLeft (localLayout, localLayout, zero))({
                    case ((lcMarker, rcMarker, ht), thisMarker) => {

                      if (! thisMarker.external) {
                        // println("Shifting " ++ thisMarker.element.owner.toString ++ " up by " ++ (localLayout.height + externalPadding).toString)
                        thisMarker.element.shiftUp(localLayout.height + externalPadding)
                        localLayout.element.verticalDependants :+= thisMarker.element
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

          // println("Finished interior layout with: " ++ layout.toString)

          // Set interior margins
          rm.leftInteriorMargin = layout.leftMargin
          rm.rightInteriorMargin = layout.rightMargin
          rm.interiorHeight = layout.height

          // Even if the layout was external  (meaning that we just
          // rendered a loop) we need to move it horizontally
          rm.horizontalDependants :+= layout.element

          if (! layout.external) {
            layout.element.shiftUp(strokeWidth + rm.labelContainerHeight)
            rm.verticalDependants :+= layout.element
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

          // println("Rendered box: " ++ rm.toString)

          marker

        }
      }
    }


}
