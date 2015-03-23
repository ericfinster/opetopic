/**
  * Render.scala - Rendering Routines
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scalaz.std.option._

import Nat._
import Tree._
import Nesting._
import Zippers._
import Complex._
import ComplexZipper._

trait Renderer[U]  {

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

    def outgoingEdgeMarker : Option[RenderMarker]

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

    override def toString = "LM" ++ 
      "(we = " ++ external.toString ++ ", ht = " ++ height.toString ++
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

  def renderObjectNesting(nst : Nesting[_0, RenderMarker]) : RenderMarker = 
    nst match {
      case Obj(rm) => {
        rm.leftInteriorMargin = zero
        rm.rightInteriorMargin = zero
        rm.interiorHeight = zero
        rm
      }
      case Box(rm, Pt(n)) => {

        val internalMarker = renderObjectNesting(n)

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

  def renderNesting[N <: Nat](nst : Nesting[S[N], RenderMarker], lvs : Tree[N, LayoutMarker]) : Option[LayoutMarker] = 
    nst match {
      case Dot(rm, d) => 
        for {
          outgoingEdge <- rm.outgoingEdgeMarker
        } yield {

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

              // println("Mid marker is: " ++ midMarker.toString)

              if (isOdd) {

                // println("Dot coords: (" ++ dot.rootX.toString ++ ", " ++ dot.rootY.toString ++ ", " ++
                //   dot.width.toString ++ ", " ++ dot.height.toString ++ ")")

                midMarker.rootEdgeMarker.edgeEndX = rm.rootX
                midMarker.rootEdgeMarker.edgeEndY = rm.rootY - rm.height

                val edgeMarker = new EdgeEndMarker(midMarker.rootEdgeMarker)

                rm.horizontalDependants :+= edgeMarker
                rm.verticalDependants :+= edgeMarker

                rm.horizontalDependants :+= midMarker.element

              }

              if (leafCount > 1) {

                val leftChildren = leafMarkers.slice(0, leafCount / 2)
                val rightChildren = leafMarkers.slice(leafCount / 2 + (leafCount & 1), leafCount)

                val leftChild = leftChildren.last
                val rightChild = rightChildren.head

                val midLeftOffset = if (isOdd) midMarker.leftMargin else zero
                val midRightOffset = if (isOdd) midMarker.rightMargin else zero

                val leftChildShift = max(max(midLeftOffset, leftChild.rightMargin + externalPadding), rm.leftMargin)
                val rightChildShift = max(max(midRightOffset, rightChild.leftMargin + externalPadding), rm.rightMargin)

                // println("Left child shift: " ++ leftChildShift.toString)
                // println("Right child shift: " ++ rightChildShift.toString)

                val leftEdge =
                  (leftChildren foldRight leftChildShift)({
                    case (currentMarker, leftShift) => {

                      // println("Passing marker: " ++ currentMarker.toString)

                      val thisShift = leftShift + externalPadding + currentMarker.rightMargin

                      // println("Shifting " ++ currentMarker.element.owner.toString ++ " left by " ++ thisShift.toString)

                      currentMarker.element.shiftLeft(thisShift)
                      rm.horizontalDependants :+= currentMarker.element

                      val edgeMarker = new EdgeEndMarker(currentMarker.rootEdgeMarker)

                      currentMarker.rootEdgeMarker.edgeEndX = rm.x
                      currentMarker.rootEdgeMarker.edgeEndY = rm.y + strokeWidth + internalPadding + rm.halfLabelHeight

                      rm.horizontalDependants :+= edgeMarker
                      rm.verticalDependants :+= edgeMarker

                      thisShift + currentMarker.leftMargin

                    }
                  })

                val rightEdge =
                  (rightChildren foldLeft rightChildShift)({
                    case (rightShift, currentMarker) => {

                      // println("Passing marker: " ++ currentMarker.toString)

                      val thisShift = rightShift + externalPadding + currentMarker.leftMargin

                      // println("Shifting " ++ currentMarker.element.owner.toString ++ " right by " ++ thisShift.toString)

                      currentMarker.element.shiftRight(thisShift)
                      rm.horizontalDependants :+= currentMarker.element

                      val edgeMarker = new EdgeEndMarker(currentMarker.rootEdgeMarker)

                      currentMarker.rootEdgeMarker.edgeEndX = rm.x + rm.width
                      currentMarker.rootEdgeMarker.edgeEndY = rm.y + strokeWidth + internalPadding + rm.halfLabelHeight

                      rm.horizontalDependants :+= edgeMarker
                      rm.verticalDependants :+= edgeMarker

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

          // println("Rendered dot with label " ++ a.toString)

          marker
        }

      case Box(rm, cn) => {

        val (leafCount : Int, leavesWithIndices : Tree[N, (LayoutMarker, Int)]) = lvs.zipWithIndex

        def verticalPass(tr : Tree[S[N], Nesting[S[N], RenderMarker]]) : Option[LayoutMarker] = 
          (new TreeGraftElim[N, Nesting[S[N], RenderMarker], LayoutMarker] {

            def caseLeaf(addr : Address[N]) : Option[LayoutMarker] = 
              for {
                leafMarkerWithIndex <- leavesWithIndices valueAt addr 
              } yield {

                val (leafMarker, leafIndex) = leafMarkerWithIndex

                if (leafIndex == 0 && leafCount == 1) {
                  leafMarker.truncateUnique
                } else if (leafIndex == 0) {
                  leafMarker.truncateLeft
                } else if (leafIndex == leafCount - 1) {
                  leafMarker.truncateRight
                } else {
                  leafMarker.truncateMiddle
                }

              }


            def caseNode(sn : Nesting[S[N], RenderMarker], layoutTree : Tree[N, LayoutMarker]) : Option[LayoutMarker] = 
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

          })(tr)

        for {
          layout <- verticalPass(cn)
        } yield {

          // Set interior margins
          rm.leftInteriorMargin = layout.leftMargin
          rm.rightInteriorMargin = layout.rightMargin
          rm.interiorHeight = layout.height

          if (! layout.external) {
            layout.element.shiftUp(strokeWidth + rm.labelContainerHeight)
            rm.verticalDependants :+= layout.element
            rm.horizontalDependants :+= layout.element
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
