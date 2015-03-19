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

abstract class Renderer[T, A](implicit isNumeric : Numeric[T]) {

  import isNumeric._

  //============================================================================================
  // RENDERING OPTIONS
  //

  def halfLeafWidth : T
  def halfStrokeWidth : T

  def strokeWidth = fromInt(2) * halfStrokeWidth
  def leafWidth = fromInt(2) * halfLeafWidth

  def internalPadding : T
  def externalPadding : T

  //============================================================================================
  // CUSTOM TYPES
  //

  type CanvasType <: NestingCanvas

  type BoxType <: LabeledBox
  type ExternalBoxType <: BoxType with ExternalBox
  type InternalBoxType <: BoxType with InternalBox

  type EdgeType <: Edge

  //============================================================================================
  // ELEMENT CONSTRUCTORS
  //

  def createNestingCanvas : NestingCanvas

  trait NestingCanvas {

    def createExternalBox(lbl : A) : ExternalBoxType
    def createInternalBox(lbl : A, layout : BoxLayout) : InternalBoxType

    def createEdge(lbl : A) : EdgeType

    def createEdgeLayout(lbl : A) : LayoutMarker =
      new LayoutMarker {

        val rootEdge = createEdge(lbl)

        val element = new EdgeStartMarker(rootEdge)
        val external = true

        override def leftInternalMargin = halfLeafWidth
        override def rightInternalMargin = halfLeafWidth

      }

    def initializeRenderPass : Unit = ()
    def finalizeRenderPass : Unit = ()


  }

  //============================================================================================
  // ELEMENT CLASSES
  //

  trait Rooted {

    val owner : A

    var rootX : T
    var rootY : T 

    var horizontalDependants : List[Rooted] = Nil
    var verticalDependants : List[Rooted] = Nil

    def shiftRight(amount : T) : Unit = {
      if (amount != 0) {
        rootX = (rootX + amount)
        horizontalDependants foreach (_.shiftRight(amount))
      }
    }

    def shiftLeft(amount : T) : Unit = {
      if (amount != 0) {
        rootX = (rootX - amount)
        horizontalDependants foreach (_.shiftLeft(amount))
      }
    }

    def shiftDown(amount : T) : Unit = {
      if (amount != 0) {
        rootY = (rootY + amount)
        verticalDependants foreach (_.shiftDown(amount))
      }
    }

    def shiftUp(amount : T) : Unit = {
      if (amount != 0) {
        rootY = (rootY - amount)
        verticalDependants foreach (_.shiftUp(amount))
      }
    }

  }

  trait LabeledBox extends BoxLayout with Rooted {

    def x : T = rootX - leftMargin
    def y : T = rootY - height

    def halfLabelWidth : T
    def halfLabelHeight : T

    def labelWidth : T = halfLabelWidth * fromInt(2)
    def labelHeight : T = halfLabelHeight * fromInt(2)

    def labelContainerWidth : T = halfLeafWidth + labelWidth + (fromInt(2) * internalPadding)
    def labelContainerHeight : T = fromInt(2) * internalPadding + labelHeight

    override def toString =
      "Box(" ++ owner.toString ++ ")(" ++ x.toString ++ ", " ++ y.toString ++ ", " ++ width.toString ++ ", " ++ height.toString ++ ")"

  }

  trait InternalBox extends LabeledBox {

    def interior : BoxLayout

    override def height : T =
      strokeWidth +
    labelContainerHeight +
    interior.height +
    internalPadding +
    strokeWidth

    override def leftMargin : T =
      interior.leftMargin +
    internalPadding +
    (fromInt(2) * strokeWidth)

    override def rightMargin : T =
      max(
        labelContainerWidth + strokeWidth,
        interior.rightMargin + internalPadding + (fromInt(2) * strokeWidth)
      )

  }

  trait ExternalBox extends LabeledBox {

    override def height : T =
      strokeWidth +
    internalPadding +
    labelHeight +
    internalPadding +
    strokeWidth

    override def leftMargin : T = halfLabelWidth + internalPadding + strokeWidth
    override def rightMargin : T = halfLabelWidth + internalPadding + strokeWidth

  }

  trait Edge {

    val owner : A

    var startX : T = zero
    var startY : T = zero

    var endX : T = zero
    var endY : T = zero

    override def toString = 
      "Edge (" ++ owner.toString ++ ")(" ++ startX.toString ++ ", " ++ startY.toString ++ 
        ") -> (" ++ endX.toString ++ ", " ++ endY.toString ++ ")"

  }

  class EdgeStartMarker(edge : Edge) extends Rooted {

    val owner : A = edge.owner

    def rootX : T = edge.startX
    def rootX_=(t : T) = edge.startX = t

    def rootY : T = edge.startY
    def rootY_=(t : T) = edge.startY = t

    // override def shiftLeft(amount : T) = {
    //   println("Shifting start of edge " ++ edge.owner.toString ++ " left by " ++ amount.toString)
    //   super.shiftLeft(amount)
    // }

    // override def shiftRight(amount : T) = {
    //   println("Shifting start of edge " ++ edge.owner.toString ++ " right by " ++ amount.toString)
    //   super.shiftRight(amount)
    // }

    // override def shiftUp(amount : T) = {
    //   // println("Shifting start of edge " ++ edge.owner.toString ++ " up by " ++ amount.toString)
    //   super.shiftUp(amount)
    // }

  }

  class EdgeEndMarker(edge : Edge) extends Rooted {

    val owner : A = edge.owner

    def rootX : T = edge.endX
    def rootX_=(t : T) = edge.endX = t

    def rootY : T = edge.endY
    def rootY_=(t : T) = edge.endY = t

    // override def shiftLeft(amount : T) = {
    //   println("Shifting end of edge " ++ edge.owner.toString ++ " left by " ++ amount.toString)
    //   super.shiftLeft(amount)
    // }

    // override def shiftRight(amount : T) = {
    //   println("Shifting end of edge " ++ edge.owner.toString ++ " right by " ++ amount.toString)
    //   super.shiftRight(amount)
    // }

    // override def shiftUp(amount : T) = {
    //   // println("Shifting end of edge " ++ edge.owner.toString ++ " up by " ++ amount.toString)
    //   super.shiftUp(amount)
    // }

  }

  trait BoxLayout {

    def width : T = leftMargin + rightMargin
    def height : T

    def leftMargin : T
    def rightMargin : T

  }


  //============================================================================================
  // COMPLEX RENDERING
  //

  def renderComplex[N <: Nat](cmplx : Complex[N, A]) : Option[Complex[N, BoxType]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = Complex[N, A] => Option[Complex[N, BoxType]]

      def caseZero : Out[_0] = {
        case (tl >>> hd) => {
          println("========= Dimension 0 =========")
          val canvas = createNestingCanvas
          val resultComplex = renderObjectNesting(hd, canvas)
          canvas.finalizeRenderPass
          Some(CNil() >>> resultComplex)
        }
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (tl >>> hd) => 
          for {
            tailResult <- renderComplex(tl)
            _ = println("========= Dimension " ++ natToInt(tl.length).toString ++  " =========")
            canvas = createNestingCanvas
            leaves <- focusSpine(fromComplex(tailResult))
            edges = map(leaves)((lb : LabeledBox) => canvas.createEdgeLayout(lb.owner))
            _ = println("About to render head")
            headResult <- renderNesting(hd, canvas, edges)
          } yield {

            println("Head render complete")

            val (layout, nesting) = headResult
            val baseBox = baseValue(nesting)

            // Should be a foreach ...
            map(edges)(m => { m.rootEdge.startY = baseBox.y - (fromInt(2) * externalPadding) })
            layout.rootEdge.endY = baseBox.rootY + (fromInt(2) * externalPadding)

            canvas.finalizeRenderPass

            (tailResult >>> nesting)
          }
      }

    })(cmplx.dim)(cmplx)

  //============================================================================================
  // OBJECT NESTING RENDERING
  //

  def renderObjectNesting(nst : Nesting[_0, A], canvas : NestingCanvas) : Nesting[_0, BoxType] = 
    nst match {
      case Obj(a) => Obj(canvas.createExternalBox(a))
      case Box(a, Pt(c)) => {

        val interior = renderObjectNesting(c, canvas)

        val internalBox = baseValue(interior)
        val box = canvas.createInternalBox(a, internalBox)

        internalBox.shiftUp(strokeWidth + box.labelContainerHeight)

        box.horizontalDependants :+= internalBox
        box.verticalDependants :+= internalBox

        Box(box, Pt(interior))

      }
    }

  //============================================================================================
  // POSITIVE DIMENSIONAL NESTING RENDERING
  //

  def renderNesting[N <: Nat](nst : Nesting[S[N], A], canvas : NestingCanvas, lvs : Tree[N, LayoutMarker])
      : Option[(LayoutMarker, Nesting[S[N], BoxType])] =
    nst match {
      case Dot(a, c) => {

        val dot = canvas.createExternalBox(a)
        val edge = canvas.createEdge(a)

        val newEdgeMarker = new EdgeStartMarker(edge)

        dot.horizontalDependants :+= newEdgeMarker
        dot.verticalDependants :+= newEdgeMarker

        val leafMarkers = lvs.nodes
        val leafCount = leafMarkers.length

        val marker =
          if (leafCount == 0) {  // This is a drop. Simply return an appropriate marker ...

            new LayoutMarker {

              val element = (dot : Rooted)
              val external = false

              val rootEdge = edge

              override def height = dot.height
              override def leftInternalMargin = dot.leftMargin 
              override def rightInternalMargin = dot.rightMargin 

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

              midMarker.rootEdge.endX = dot.rootX
              midMarker.rootEdge.endY = dot.rootY - dot.height

              val edgeMarker = new EdgeEndMarker(midMarker.rootEdge)

              dot.horizontalDependants :+= edgeMarker
              dot.verticalDependants :+= edgeMarker

              dot.horizontalDependants :+= midMarker.element

            }

            if (leafCount > 1) {

              val leftChildren = leafMarkers.slice(0, leafCount / 2)
              val rightChildren = leafMarkers.slice(leafCount / 2 + (leafCount & 1), leafCount)

              val leftChild = leftChildren.last
              val rightChild = rightChildren.head

              val midLeftOffset = if (isOdd) midMarker.leftMargin else zero
              val midRightOffset = if (isOdd) midMarker.rightMargin else zero

              val leftChildShift = max(max(midLeftOffset, leftChild.rightMargin + externalPadding), dot.leftMargin)
              val rightChildShift = max(max(midRightOffset, rightChild.leftMargin + externalPadding), dot.rightMargin)

              // println("Left child shift: " ++ leftChildShift.toString)
              // println("Right child shift: " ++ rightChildShift.toString)

              val leftEdge =
                (leftChildren foldRight leftChildShift)({
                  case (currentMarker, leftShift) => {

                    // println("Passing marker: " ++ currentMarker.toString)

                    val thisShift = leftShift + externalPadding + currentMarker.rightMargin

                    // println("Shifting " ++ currentMarker.element.owner.toString ++ " left by " ++ thisShift.toString)

                    currentMarker.element.shiftLeft(thisShift)
                    dot.horizontalDependants :+= currentMarker.element

                    val edgeMarker = new EdgeEndMarker(currentMarker.rootEdge)

                    currentMarker.rootEdge.endX = dot.x
                    currentMarker.rootEdge.endY = dot.y + strokeWidth + internalPadding + dot.halfLabelHeight

                    dot.horizontalDependants :+= edgeMarker
                    dot.verticalDependants :+= edgeMarker

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
                    dot.horizontalDependants :+= currentMarker.element

                    val edgeMarker = new EdgeEndMarker(currentMarker.rootEdge)

                    currentMarker.rootEdge.endX = dot.x + dot.width
                    currentMarker.rootEdge.endY = dot.y + strokeWidth + internalPadding + dot.halfLabelHeight

                    dot.horizontalDependants :+= edgeMarker
                    dot.verticalDependants :+= edgeMarker

                    thisShift + currentMarker.rightMargin

                  }
                })

            }

            new LayoutMarker {

              val element = (dot : Rooted)
              val external = false

              val rootEdge = edge

              override def height = dot.height 

              override def leftInternalMargin =
                if (firstMarker.element.rootX < dot.x) {
                  (dot.rootX - firstMarker.element.rootX) + halfLeafWidth
                } else {
                  dot.leftMargin
                }

              override def rightInternalMargin =
                if (lastMarker.element.rootX > (dot.x + dot.width)) {
                  (lastMarker.element.rootX - dot.rootX) + halfLeafWidth
                } else {
                  dot.rightMargin
                }

              override def leftSubtreeMargin = 
                if (firstMarker.element.rootX < dot.x) {
                  firstMarker.leftMargin - halfLeafWidth
                } else zero

              override def rightSubtreeMargin = 
                if (lastMarker.element.rootX > (dot.x + dot.width)) {
                  lastMarker.rightMargin - halfLeafWidth
                } else zero

            }

          }

        // println("Rendered dot with label " ++ a.toString)

        Some(marker, Dot(dot, c))

      }

      case Box(a, c) => {

        val (leafCount : Int, leavesWithIndices : Tree[N, (LayoutMarker, Int)]) = lvs.zipWithIndex

        def verticalPass(tr : Tree[S[N], Nesting[S[N], A]]) : Option[(LayoutMarker, Tree[S[N], Nesting[S[N], BoxType]])] = {

          type ResultType = (LayoutMarker, Tree[S[N], Nesting[S[N], BoxType]])

          object verticalRecursor extends TreeGraftElim[N, Nesting[S[N], A], ResultType] {

            def caseLeaf(addr : Address[N]) : Option[ResultType] =
              for {
                leafMarkerWithIndex <- leavesWithIndices valueAt addr // Have to do a traverse with an address ...
              } yield {

                val (leafMarker, leafIndex) = leafMarkerWithIndex

                if (leafIndex == 0 && leafCount == 1) {
                  (leafMarker.truncateUnique, Leaf(tr.dim))
                } else if (leafIndex == 0) {
                  (leafMarker.truncateLeft, Leaf(tr.dim))
                } else if (leafIndex == leafCount - 1) {
                  (leafMarker.truncateRight, Leaf(tr.dim))
                } else {
                  (leafMarker.truncateMiddle, Leaf(tr.dim))
                }

              }

            def caseNode(sn : Nesting[S[N], A], sh : Tree[N, ResultType]) : Option[ResultType] = {

              val (layoutTree, resultTree) = unzip(sh)

              for {
                // vresult <- traverse(vns)(verticalPass(_))
                // (layoutTree, resultTree) = unzip(vresult)
                lresult <- renderNesting(sn, canvas, layoutTree)
                (localLayout, resultNesting) = lresult
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

                  val rootEdge = localLayout.rootEdge

                  override def height = localLayout.height + externalPadding + heightOfChildren

                  override def leftInternalMargin =
                    (localLayout.element.rootX  - leftMostChild.element.rootX) + leftMostChild.leftInternalMargin

                  override def rightInternalMargin =
                    (rightMostChild.element.rootX - localLayout.element.rootX) + rightMostChild.rightInternalMargin

                  override def leftSubtreeMargin = leftMostChild.leftSubtreeMargin
                  override def rightSubtreeMargin = rightMostChild.rightSubtreeMargin

                }

                (marker, Node(resultNesting, resultTree))

              }
            }

          }

          graftElim(tr)(verticalRecursor)

        }

        for {
          internalResult <- verticalPass(c)
        } yield {

          val (layout, canopy) = internalResult

          val box = canvas.createInternalBox(a, layout)

          if (! layout.external) {
            // println("Shifting " ++ layout.element.owner.toString ++ " up by " ++ (strokeWidth + box.labelContainerHeight).toString)
            layout.element.shiftUp(strokeWidth + box.labelContainerHeight)
            box.verticalDependants :+= layout.element
            box.horizontalDependants :+= layout.element
          }

          // Setup and return an appropriate marker
          val marker = new LayoutMarker {

            val element = (box : Rooted)
            val external = false

            val rootEdge = layout.rootEdge

            override def height = box.height
            override def leftInternalMargin = box.leftMargin 
            override def rightInternalMargin = box.rightMargin

          }

          // println("Rendered box with label " ++ a.toString)

          (marker , Box(box, canopy))

        }
      }
    }


  //============================================================================================
  // LAYOUT CLASS
  //

  abstract class LayoutMarker extends BoxLayout { thisMarker =>

    val element : Rooted
    val external : Boolean

    val rootEdge : EdgeType

    def height : T = zero

    def leftSubtreeMargin : T = zero
    def rightSubtreeMargin : T = zero
    def leftInternalMargin : T = zero
    def rightInternalMargin : T = zero

    def leftMargin : T = leftSubtreeMargin + leftInternalMargin 
    def rightMargin : T = rightSubtreeMargin + rightInternalMargin

    def leftEdge : T = element.rootX - leftMargin
    def rightEdge : T = element.rootX + rightMargin

    // Truncations

    def truncateLeft : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val rootEdge = thisMarker.rootEdge
        val external = true
        override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
        override def rightInternalMargin = thisMarker.rightInternalMargin
      }

    def truncateRight : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val rootEdge = thisMarker.rootEdge
        val external = true
        override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
        override def leftInternalMargin = thisMarker.leftInternalMargin
      }

    def truncateUnique : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val rootEdge = thisMarker.rootEdge
        val external = true
      }

    def truncateMiddle : LayoutMarker =
      new LayoutMarker {
        val element = thisMarker.element
        val rootEdge = thisMarker.rootEdge
        val external = true
        override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
        override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
        override def leftInternalMargin = thisMarker.leftInternalMargin
        override def rightInternalMargin = thisMarker.rightInternalMargin
      }

    override def toString = "LM(" ++ element.owner.toString ++ ")" ++ 
      "(we = " ++ external.toString ++ ", ht = " ++ height.toString ++
      ", rx = " ++ element.rootX.toString ++
      ", ry = " ++ element.rootY.toString ++
      ", lsm = " ++ leftSubtreeMargin.toString ++
      ", lim = " ++ leftInternalMargin.toString ++
      ", rim = " ++ rightInternalMargin.toString ++
      ", rsm = " ++ rightSubtreeMargin.toString ++ ")"

  }

}
