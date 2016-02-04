/**
  * Panel.scala - Opetopic Panels
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

import scala.collection.mutable.ListBuffer

import opetopic._
import syntax.tree._
import syntax.nesting._

trait HasPanels { self : UIFramework =>

  import isNumeric._

  case class PanelConfig(
    val internalPadding : Size,
    val externalPadding : Size,
    val decorationPadding : Size,
    val leafWidth : Size,
    val strokeWidth : Size,
    val cornerRadius : Size
  )

  trait Panel[A, N <: Nat] extends BoundedElement {

    val config : PanelConfig
    import config._

    implicit def panelDim: N

    def halfLeafWidth : Size = half(leafWidth)
    def halfStrokeWidth : Size = half(strokeWidth)

    def nesting: Nesting[A, N]
    def boxNesting: Nesting[BoxType, N]

    def visualize(a: A) : Visualization[N]

    //============================================================================================
    // ADDRESSING
    //

    type PanelAddressType

    def cellAtAddress(addr: PanelAddressType) : ShapeM[BoxType] = 
      for { z <- seekToAddress(addr) } yield { z._1.baseValue }
      
    def seekToAddress(addr: PanelAddressType) : ShapeM[NestingZipper[BoxType, N]]

    //============================================================================================
    // BOX AND EDGE CONSTRUCTORS
    //

    type BoxType <: CellBox[A, N] { type BoxAddressType = PanelAddressType }
    type EdgeType <: CellEdge[A, N]

    def cellEdge : EdgeType

    //============================================================================================
    // LAYOUT MARKER HELPER CLASS
    //


    // Why don't you clean this up and make it a case class?  This complicated version
    // is really unnecessary .....

    abstract class LayoutMarker { thisMarker =>

      val element : Rooted
      val exterior : Boolean  // This is really a bad name for this .. you should change it

      val rootEdge : CellEdge[A, N]

      def height : Size = zero

      def leftSubtreeMargin : Size = zero
      def rightSubtreeMargin : Size = zero
      def leftInternalMargin : Size = zero
      def rightInternalMargin : Size = zero

      def leftMargin : Size = leftSubtreeMargin + leftInternalMargin
      def rightMargin : Size = rightSubtreeMargin + rightInternalMargin

      def leftEdge : Size = element.rootX - leftMargin
      def rightEdge : Size = element.rootX + rightMargin

      // Truncations

      def truncateLeft : LayoutMarker =
        new LayoutMarker {
          val element = thisMarker.element
          val exterior = true
          val rootEdge = thisMarker.rootEdge
          override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
          override def rightInternalMargin = thisMarker.rightInternalMargin
        }

      def truncateRight : LayoutMarker =
        new LayoutMarker {
          val element = thisMarker.element
          val exterior = true
          val rootEdge = thisMarker.rootEdge
          override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
          override def leftInternalMargin = thisMarker.leftInternalMargin
        }

      def truncateUnique : LayoutMarker =
        new LayoutMarker {
          val element = thisMarker.element
          val rootEdge = thisMarker.rootEdge
          val exterior = true
        }

      def truncateMiddle : LayoutMarker =
        new LayoutMarker {
          val element = thisMarker.element
          val exterior = true
          val rootEdge = thisMarker.rootEdge
          override def leftSubtreeMargin = thisMarker.leftSubtreeMargin
          override def rightSubtreeMargin = thisMarker.rightSubtreeMargin
          override def leftInternalMargin = thisMarker.leftInternalMargin
          override def rightInternalMargin = thisMarker.rightInternalMargin
        }

      override def toString = "LM(" ++ element.toString ++ ")" ++
      "(we = " ++ exterior.toString ++ ", ht = " ++ height.toString ++
      ", re = " ++ rootEdge.toString ++
      ", rx = " ++ element.rootX.toString ++
      ", ry = " ++ element.rootY.toString ++
      ", lsm = " ++ leftSubtreeMargin.toString ++
      ", lim = " ++ leftInternalMargin.toString ++
      ", rim = " ++ rightInternalMargin.toString ++
      ", rsm = " ++ rightSubtreeMargin.toString ++ ")"

    }

    //============================================================================================
    // EDGE MARKERS
    //

    class EdgeStartMarker(edge : CellEdge[A, N]) extends Rooted {

      def rootX : Size = edge.edgeStartX
      def rootX_=(u : Size) : Unit =
        edge.edgeStartX = u

      def rootY : Size = edge.edgeStartY
      def rootY_=(u : Size) : Unit =
        edge.edgeStartY = u

    }

    class EdgeEndMarker(edge : CellEdge[A, N]) extends Rooted {

      def rootX : Size = edge.edgeEndX
      def rootX_=(u : Size) : Unit =
        edge.edgeEndX = u

      def rootY : Size = edge.edgeEndY
      def rootY_=(u : Size) : Unit =
        edge.edgeEndY = u

    }

  }

  //============================================================================================
  // CELL BOXES
  //

  // The box should already contain a component for the label ...
  trait CellBox[A, N <: Nat] extends Rooted with BoundedElement {

    type PanelType <: Panel[A, N]
    type BoxAddressType

    val panel: PanelType
    import panel.config._

    def label: A
    def boxDim: N = panel.panelDim
    def address: BoxAddressType
    def nestingAddress: Address[S[N]]
    def isExternal: Boolean
    def visualization : Visualization[N]

    def labelElement : Element = visualization.labelElement.element
    def labelBounds : Bounds = visualization.labelElement.bounds
    def colorSpec : ColorSpec = visualization.colorSpec

    def bounds: Bounds = Bounds(x, y, width, height)

    //
    // Mutable Values
    //

    var rootX : Size = zero
    var rootY : Size = zero

    var leftInteriorMargin : Size = zero
    var rightInteriorMargin : Size = zero

    var interiorHeight : Size = zero

    // Now that this exists outside and with an index parameter, we should
    // be able to fix things up so that it's not an option in the correct dimensions ..
    var outgoingEdge : Option[CellEdge[A, N]] = None

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
      horizontalDependants.clear
      verticalDependants.clear
    }

  }

  //============================================================================================
  // CELL EDGES
  //


  trait CellEdge[A, N <: Nat] extends BoundedElement {

    val panel: Panel[A, N]
    import panel.config._

    var edgeStartX : Size = zero
    var edgeStartY : Size = zero

    var edgeEndX : Size = zero
    var edgeEndY : Size = zero

    val edgeDecorations : ListBuffer[DecorationMarker] = ListBuffer.empty

    class DecorationMarker(val be: BoundedElement, initX: Size, initY: Size) extends Rooted {

      edgeDecorations += this

      var rootX : Size = initX
      var rootY : Size = initY

    }

    def bounds: Bounds = {

      // Hmm.  But this may no longer be true:
      // We have to check all the decorations.

      val (x, width) =
        if (isOrdered.gt(edgeEndX, edgeStartX)) {
          (edgeStartX, edgeEndX - edgeStartX)
        } else {
          (edgeStartX, edgeStartX - edgeEndX)
        }

      Bounds(x, edgeStartY, width, edgeEndY - edgeStartY)

    }

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

  }

  //============================================================================================
  // OBJECT PANELS
  //

  trait ObjectPanel[A] extends Panel[A, _0] {

    import config._

    def layoutObjects(nst : Nesting[BoxType, _0]) : BoxType =
      nst match {
        case Obj(b) => { b.clear ; b }
        case Box(b, Pt(n)) => {

          val internalMarker = layoutObjects(n)

          b.clear

          b.leftInteriorMargin = internalMarker.leftMargin
          b.rightInteriorMargin = internalMarker.rightMargin
          b.interiorHeight = internalMarker.height + internalPadding

          internalMarker.shiftUp(strokeWidth + b.labelHeight + internalPadding + internalPadding)

          b.horizontalDependants += internalMarker
          b.verticalDependants += internalMarker

          b

        }
      }

  }

  //============================================================================================
  // NESTING PANEL
  //

  trait NestingPanel[A, P <: Nat] extends Panel[A, S[P]] {

    import config._

    def edgeNesting: Nesting[EdgeType, P]

    // This terrible hack happens because boxes are not correctly
    // indexed over dimension.  Eventually you have to fix this, but
    // honestly, right now, I just don't have the heart.
    def cellVisualization(box: BoxType) : CellVisualization[P] = 
      box.visualization.asInstanceOf[CellVisualization[P]]

    //============================================================================================
    // EDGE NESTING GENERATION
    //

    def connectEdges(en: Nesting[EdgeType, P], nst: Nesting[BoxType, S[P]]) : Nesting[EdgeType, P] = {

      nst match {
        case Dot(mk, _) => mk.outgoingEdge = Some(en.baseValue)
        case Box(_, cn) => {
          // Here we match the two guys ....
          for {
            spine <- Nesting.spineFromCanopy(cn)
            res <- spine.mapWith(en.toTree)({
              case (mk, edge) => mk.outgoingEdge = Some(edge)
            })
          } yield ()
        }
      }

      en

    }

    def reconstructEdges(p: P)(nst: Nesting[BoxType, S[P]]) : Nesting[EdgeType, P] = {

      // Hmmm. In the external case, the nesting does not contain leaf information ....
      // So the panel cannot know what to do without some kind of help ....
      // So we should have an optional constructor which gives a hint in this case ...

      val edgeComp =
        nst match {
          case Dot(_, sp) => fail("Base nesting is external!")
          case Box(_, cn) =>
            for {
              spine <- Nesting.spineFromCanopy(cn)
              res <- Tree.graftRec(p)(spine)(ad => {
                val edge = cellEdge
                succeed(Nesting.external(p)(edge))
              })({ case (mk, cn) => {
                val edge = cellEdge
                mk.outgoingEdge = Some(edge)
                succeed(Box(edge, cn))
              }})
            } yield res
        }

      import scalaz.-\/
      import scalaz.\/-

      edgeComp match {
        case \/-(enst) => enst
        case _ => {
          // A dummy return, since this is an error ...
          Nesting.external(p)(cellEdge)
        }
      }

    }

    def edgeLayoutTree(en: Nesting[EdgeType, P]) : ShapeM[Tree[LayoutMarker, P]] =
      for {
        spine <- Nesting.spineFromDerivative(en, Zipper.globDerivative(en.dim))
      } yield spine.map(edge => {
        new LayoutMarker {

          val element = new EdgeStartMarker(edge)
          val rootEdge = edge
          val exterior = true

          override def leftInternalMargin = halfLeafWidth
          override def rightInternalMargin = halfLeafWidth

        }
      })

    //============================================================================================
    // LAYOUT
    //

    def layout(nst: Nesting[BoxType, S[P]], et: Nesting[EdgeType, P]) : Unit =
      for {
        elt <- edgeLayoutTree(et)
        mk <- layoutNesting(nst, elt)
      } {

        val baseBox = nst.baseValue
        val localVis : CellVisualization[P] =
          cellVisualization(baseBox)

        val incomingLeafInfo = 
          ((for {
            lees <- localVis.leafEdgeElements
            ztr <- toOpt(Tree.matchTraverse(elt, lees)(
              (l: LayoutMarker, o: Option[BoundedElement]) => succeed((l, o))
            ))
          } yield ztr) getOrElse (elt map (l => (l, None)))).nodes

        for {
          (em , decOpt) <- incomingLeafInfo
        } {

          em.rootEdge.edgeStartY = baseBox.y - (fromInt(2) * externalPadding)

          decOpt match {
            case None => ()
            case Some(be) => {
              val re = em.rootEdge
              val decMkr = new re.DecorationMarker(be, re.edgeStartX, -baseBox.height - decorationPadding)
              mk.element.horizontalDependants += decMkr
              mk.element.verticalDependants += decMkr
            }
          }

        }

        // Set the position of the outgoing edge
        mk.rootEdge.edgeEndY = baseBox.rootY + (fromInt(2) * externalPadding)

        // Position the root edge decoration
        localVis.rootEdgeElement match {
          case None => ()
          case Some(be) => {
            val re = mk.rootEdge
            val decMkr = new re.DecorationMarker(be, re.edgeStartX, decorationPadding)
            mk.element.horizontalDependants += decMkr
            mk.element.verticalDependants += decMkr
          }
        }

      }

    def layoutNesting(nst : Nesting[BoxType, S[P]], lvs : Tree[LayoutMarker, P]) : ShapeM[LayoutMarker] =
      nst match {
        case Dot(bx, d) =>
          for {
            outgoingEdge <- fromOpt(
              bx.outgoingEdge,
              new ShapeError("Missing outgoing edge for" ++ bx.toString)
            )
          } yield {

            bx.clear

            val newEdgeMarker = new EdgeStartMarker(outgoingEdge)

            bx.horizontalDependants += newEdgeMarker
            bx.verticalDependants += newEdgeMarker

            val leafMarkers = lvs.nodes
            val leafCount = leafMarkers.length

            // Zeroed out for external dots
            bx.leftInteriorMargin = zero
            bx.rightInteriorMargin = zero
            bx.interiorHeight = zero

            val marker =
              if (leafCount == 0) {  // This is a drop. Simply return an appropriate marker ...

                new LayoutMarker {

                  val element = bx
                  val exterior = false
                  val rootEdge = outgoingEdge

                  override def height = bx.height
                  override def leftInternalMargin = bx.leftMargin
                  override def rightInternalMargin = bx.rightMargin

                }

              } else { // We have children.  Arrange them and calculate the marker.

                val isOdd = (leafCount & 1) != 0

                val firstMarker = leafMarkers.head
                val lastMarker = leafMarkers.last

                val midMarker = leafMarkers(leafCount / 2)

                if (isOdd) {

                  midMarker.rootEdge.edgeEndX = bx.rootX
                  midMarker.rootEdge.edgeEndY = bx.rootY - bx.height

                  val edgeMarker = new EdgeEndMarker(midMarker.rootEdge)

                  bx.horizontalDependants += edgeMarker
                  bx.verticalDependants += edgeMarker

                  bx.horizontalDependants += midMarker.element

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
                    bx.horizontalDependants += marker.element

                    val edgeMarker = new EdgeEndMarker(marker.rootEdge)

                    marker.rootEdge.edgeEndX = bx.x
                    marker.rootEdge.edgeEndY = bx.y + strokeWidth + internalPadding + bx.halfLabelHeight

                    bx.horizontalDependants += edgeMarker
                    bx.verticalDependants += edgeMarker

                  }

                  def doRightPlacement(marker : LayoutMarker, shift : Size) : Unit = {

                    marker.element.shiftRight(shift)
                    bx.horizontalDependants += marker.element

                    val edgeMarker = new EdgeEndMarker(marker.rootEdge)

                    marker.rootEdge.edgeEndX = bx.x + bx.width
                    marker.rootEdge.edgeEndY = bx.y + strokeWidth + internalPadding + bx.halfLabelHeight

                    bx.horizontalDependants += edgeMarker
                    bx.verticalDependants += edgeMarker

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

                  val element = bx
                  val exterior = false
                  val rootEdge = outgoingEdge

                  override def height = bx.height

                  override def leftInternalMargin =
                    if (firstMarker.element.rootX < bx.x) {
                      (bx.rootX - firstMarker.element.rootX) + halfLeafWidth
                    } else {
                      bx.leftMargin
                    }

                  override def rightInternalMargin =
                    if (lastMarker.element.rootX > (bx.x + bx.width)) {
                      (lastMarker.element.rootX - bx.rootX) + halfLeafWidth
                    } else {
                      bx.rightMargin
                    }

                  override def leftSubtreeMargin =
                    if (firstMarker.element.rootX < bx.x) {
                      firstMarker.leftMargin - halfLeafWidth
                    } else zero

                  override def rightSubtreeMargin =
                    if (lastMarker.element.rootX > (bx.x + bx.width)) {
                      lastMarker.rightMargin - halfLeafWidth
                    } else zero

                }

              }

            marker

          }

        case Box(bx, cn) => {

          bx.clear

          val (leafCount : Int, leavesWithIndices : Tree[(LayoutMarker, Int), P]) = lvs.zipWithIndex

          def verticalPass(tr : Tree[Nesting[BoxType, S[P]], S[P]]) : ShapeM[LayoutMarker] =
            Tree.graftRec[Nesting[BoxType, S[P]], LayoutMarker, P](tr)({
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

                  // Get the cell visualization info from the just finished box
                  val localVis : CellVisualization[P] = 
                    cellVisualization(sn.baseValue)

                  // Zip together the incoming markers and any decoration information
                  val descendantMarkers : List[(LayoutMarker, Option[BoundedElement])] = 
                    ((for {
                      lees <- localVis.leafEdgeElements
                      ztr <- toOpt(Tree.matchTraverse(layoutTree, lees)(
                        (l : LayoutMarker, o: Option[BoundedElement]) => succeed((l, o))
                      ))
                    } yield ztr) getOrElse (layoutTree map (l => (l, None)))).nodes

                  val (leftMostChild, rightMostChild, heightOfChildren, topShim) =
                    (descendantMarkers foldLeft (localLayout, localLayout, zero, externalPadding))({
                      case ((lcMarker, rcMarker, ht, ts), (thisMarker, thisDecOpt)) => {

                        val thisShim = 
                          thisDecOpt match {
                            case None => externalPadding
                            case Some(be) => {

                              val re = thisMarker.rootEdge
                              val decMkr = new re.DecorationMarker(be, re.edgeStartX, - localLayout.height - decorationPadding)
                              localLayout.element.horizontalDependants += decMkr
                              localLayout.element.verticalDependants += decMkr

                              be.bounds.height + decorationPadding + strokeWidth

                            }
                          }

                        if (! thisMarker.exterior) {
                          thisMarker.element.shiftUp(localLayout.height + thisShim)
                          localLayout.element.verticalDependants += thisMarker.element
                        }

                        val newLeftChild = if (thisMarker.leftEdge < lcMarker.leftEdge) thisMarker else lcMarker
                        val newRightChild = if (thisMarker.rightEdge > rcMarker.rightEdge) thisMarker else rcMarker

                        (newLeftChild, newRightChild, max(ht, thisMarker.height), max(ts, thisShim))

                      }
                    })

                  // Calculate the bottom shim necessary to clear any outgoing edge marker
                  val bottomShim =
                    localVis.rootEdgeElement match {
                      case None => zero
                      case Some(be) => {

                        val re = localLayout.rootEdge
                        val decMkr = new re.DecorationMarker(be, re.edgeStartX, decorationPadding)
                        localLayout.element.horizontalDependants += decMkr
                        localLayout.element.verticalDependants += decMkr

                        be.bounds.height + decorationPadding

                      }
                    }

                  // Shift up the local layout to make room
                  localLayout.element.shiftUp(bottomShim)

                  val marker = new LayoutMarker {

                    val element = localLayout.element
                    val exterior = false
                    val rootEdge = localLayout.rootEdge

                    override def height = bottomShim + localLayout.height + topShim + heightOfChildren

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
            bx.leftInteriorMargin = layout.leftMargin
            bx.rightInteriorMargin = layout.rightMargin
            bx.interiorHeight = layout.height

            // Even if the layout was exterior  (meaning that we just
            // rendered a loop) we need to move it horizontally
            bx.horizontalDependants += layout.element

            if (! layout.exterior) {
              layout.element.shiftUp(strokeWidth + bx.labelHeight + internalPadding + internalPadding)
              bx.verticalDependants += layout.element
            }

            // Setup and return an appropriate marker
            val marker = new LayoutMarker {

              val element = bx
              val exterior = false
              val rootEdge = layout.rootEdge

              override def height = bx.height
              override def leftInternalMargin = bx.leftMargin
              override def rightInternalMargin = bx.rightMargin

            }

            marker

          }
        }
      }

  }

  //============================================================================================
  // ROOTED HELPER TRAIT
  //

  trait Rooted {

    var rootX : Size
    var rootY : Size

    val horizontalDependants : ListBuffer[Rooted] = ListBuffer.empty
    val verticalDependants : ListBuffer[Rooted] = ListBuffer.empty

    def shiftRight(amount : Size) : Unit = {
      if (amount != 0) {
        rootX = (rootX + amount)
        horizontalDependants foreach (_.shiftRight(amount))
      }
    }

    def shiftDown(amount : Size) : Unit = {
      if (amount != 0) {
        rootY = (rootY + amount)
        verticalDependants foreach (_.shiftDown(amount))
      }
    }

    def shiftLeft(amount : Size) : Unit = shiftRight(-amount)
    def shiftUp(amount : Size) : Unit = shiftDown(-amount)

  }

  //============================================================================================
  // EDGE HELPERS
  //

  // trait EdgeLike {

  //   var edgeStartX : Size = zero
  //   var edgeStartY : Size = zero

  //   var edgeEndX : Size = zero
  //   var edgeEndY : Size = zero

  // }

}
