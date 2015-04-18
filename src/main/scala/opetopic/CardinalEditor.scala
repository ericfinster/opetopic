/**
  * CardinalEditor.scala - A Base Class for Cardinal-Type Editors
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.collection.mutable.ListBuffer

import scalaz.Applicative
import scalaz.syntax.monad._

import TypeDefs._
import Cardinal._

import syntax.tree._
import syntax.complex._
import syntax.nesting._
import syntax.cardinal._

trait CardinalEditor[A[_ <: Nat], U] extends Viewer[({ type L[K <: Nat] = Polarity[Option[A[K]]] })#L, U] { 

  type OptA[K <: Nat] = Option[A[K]]

  type MarkerType[N <: Nat] <: CardinalMarker[N]
  type NeutralMarkerType[N <: Nat] <: MarkerType[N] with NeutralMarker[N]
  type PolarizedMarkerType[N <: Nat] <: MarkerType[N] with PolarizedMarker[N]

  def complex : FiniteComplex[MarkerType] = 
    complexToFiniteComplex(editorState.complex)

  //============================================================================================
  // EDITOR STATE
  //

  var editorState : EditorState

  trait EditorState { thisState => 

    type Dim <: Nat

    val dim : Dim

    val complex : Complex[MarkerType, Dim]
    val cardinal : Cardinal[NeutralMarkerType, Dim]

    val polarizedMarkers : PolaritySuite[PolarizedMarkerType, Dim]

    val canvases : List[CanvasType]
    val nextCanvas : CanvasType

    def refreshCardinalAddresses : Unit = {
      type T[K <: Nat] = CardinalNesting[NeutralMarkerType[K], K]

      Suite.foreach[T, S[Dim]](cardinal)(new Suite.IndexedOp[T] {
        def apply[N <: Nat](n: N)(cn: T[N]) : Unit = {
          implicit val curDim = n

          // println("Cardinal address update in dimension " ++ natToInt(n).toString)

          import scalaz.Id._

          traverseCardinalTreeWithAddr(n)(cn)({ 
            case (nst, base) => nst traverseWithAddress[Id, Unit] {
              case (mk, addr) => {
                // println("Updating cardinal address for " ++ mk.toString)
                mk.cardinalAddress = Some(base >> addr)
              }
            }
          })
        }
      })

    }

    def refreshComplexAddresses : Unit = {
      type T[K <: Nat] = Nesting[MarkerType[K], K]

      Suite.foreach[T, S[Dim]](complex)(new Suite.IndexedOp[T] {
        def apply[N <: Nat](n: N)(nst: T[N]) : Unit = {

          // println("Complex address update in dimension " ++ natToInt(n).toString)

          import scalaz.Id._

          nst traverseWithAddress[Id, Unit] {
            case (mk, addr) => {
              // println("Updating complex address for " ++ mk.toString)
              mk.nestingAddress = addr
            }
          }
        }
      })
    }

    def refreshFaceComplexes : Unit = {
      type MarkerComplex[K <: Nat] = Complex[MarkerType, K]

      println("About to refresh faces ...")
      println("Complex is: " ++ complex.toString)

      import scalaz.-\/
      import scalaz.\/-

      complex.comultiply match {
        case -\/(ShapeError(str)) => println("Failed with: " ++ str)
        case \/-(dblcmplx) => {
          println("Comultiplication succeeded")
          dblcmplx.foreach(new Suite.IndexedOp[MarkerComplex] {
            def apply[N <: Nat](n: N)(mc: MarkerComplex[N]) : Unit = {
              println("Setting face complex: " ++ mc.toString)
              mc.head.baseValue.faceComplex = Some(mc)
            }
          })
        }
      }

      // for {
      //   dblcmplx <- complex.comultiply
      // } {
      //   println("Comultiplication succeeded")
      //   dblcmplx.foreach(new Suite.IndexedOp[MarkerComplex] {
      //     def apply[N <: Nat](n: N)(mc: MarkerComplex[N]) : Unit = {
      //       println("Setting face complex: " ++ mc.toString)
      //       mc.head.baseValue.faceComplex = Some(mc)
      //     }
      //   })
      // }
    }

  }

  object EditorState {

    type EditorStateAux[N <: Nat] = EditorState { type Dim = N }

    def apply(nst : Nesting[Option[A[_0]], _0]) : EditorStateAux[_0] = {

      val objCanvas = createCanvas
      val edgeCanvas = createCanvas

      displayCanvas(objCanvas)

      val posMarker : PolarizedMarkerType[_0] = 
        createPositiveMarker(Z)(objCanvas, edgeCanvas)

      def genObjData(nst: Nesting[Option[A[_0]], _0], base: Address[_1]) : Nesting[NeutralMarkerType[_0], _0] = 
        nst match {
          case Obj(opt) => {
            val marker = createNeutralMarker(Z)(opt, CardinalAddress() >> base, true, objCanvas, edgeCanvas)
            marker.faceComplex = Some(Complex() >> Obj(marker))
            Obj(marker)
          }
          case Box(opt, Pt(n)) => {
            val marker = createNeutralMarker(Z)(opt, CardinalAddress() >> base, false, objCanvas, edgeCanvas)
            marker.faceComplex = Some(Complex() >> Obj(marker))
            Box(marker, Pt(genObjData(n, () :: base)))
          }
        }

      val neutralCanopy = Pt(genObjData(nst, Nil))

      new EditorState {

        type Dim = _0

        val dim : Dim = Z

        val complex : Complex[MarkerType, Dim] = 
          Complex[MarkerType]() >> Box(posMarker, neutralCanopy)

        val cardinal : Cardinal[NeutralMarkerType, Dim] = 
          Cardinal[NeutralMarkerType]() >> neutralCanopy

        val polarizedMarkers : PolaritySuite[PolarizedMarkerType, Dim] = 
          PolaritySuite[PolarizedMarkerType]() >> (posMarker, posMarker)

        val canvases : List[CanvasType] = List(objCanvas)
        val nextCanvas : CanvasType = edgeCanvas

      }

    }

    def apply[N <: Nat](es: EditorStateAux[N], cn: CardinalNesting[Option[A[S[N]]], S[N]]) : EditorStateAux[S[N]] = {

      val nextDim = S(es.dim)

      val objCanvas = es.nextCanvas
      val edgeCanvas = createCanvas

      displayCanvas(objCanvas)

      val posMarker : PolarizedMarkerType[S[N]] = createPositiveMarker(nextDim)(objCanvas, edgeCanvas)
      val negMarker : PolarizedMarkerType[S[N]] = createNegativeMarker(nextDim)(objCanvas, edgeCanvas)

      def genNstData(nst: Nesting[Option[A[S[N]]], S[N]], pref: CardinalAddress[S[N]], base: Address[S[S[N]]]) 
          : Nesting[NeutralMarkerType[S[N]], S[N]] =
        nst match {
          case Dot(opt, d) => 
            Dot(createNeutralMarker(nextDim)(opt, pref >> base, true, objCanvas, edgeCanvas), d)
          case Box(opt, cn) => 
            Box(createNeutralMarker(nextDim)(opt, pref >> base, false, objCanvas, edgeCanvas), 
              cn mapWithAddress {
                case (nst, dir) => genNstData(nst, pref, dir :: base)
              }
            )
        }

      val neutralNesting = mapCardinalTreeWithAddr(nextDim)(cn)({
        case (nst, pref) => genNstData(nst, pref, Nil)
      })

      val neutralCanopy = Node(Dot(negMarker, nextDim), toShell(es.dim)(neutralNesting))
      val neutralBox = Box(posMarker, neutralCanopy)

      for {
        dots <- Nesting.spineFromCanopy(neutralCanopy)
        edges = es.complex.head.toTree
        _ <- dots.mapWith(edges)({
          case (dotMarker, edgeMarker) => {
            dotMarker.outgoingEdgeMarker = Some(edgeMarker)
          }
        })
      } yield ()

      val extendedComplex = es.complex >> neutralBox

      neutralBox traverseWithAddress[ShapeM, Unit] {
        case (mk, addr) => 
          for {
            fc <- extendedComplex sourceAt addr
          } yield {
            mk.nestingAddress = addr
            mk.faceComplex = Some(fc)
          }
      }

      new EditorState {

        type Dim = S[es.Dim]

        val dim = nextDim

        val complex : Complex[MarkerType, Dim] = 
          extendedComplex

        val cardinal : Cardinal[NeutralMarkerType, Dim] =
          es.cardinal >> neutralNesting

        val polarizedMarkers : PolaritySuite[PolarizedMarkerType, Dim] =
          es.polarizedMarkers >> (negMarker, posMarker)

        val canvases : List[CanvasType] =
          es.canvases :+ objCanvas

        val nextCanvas : CanvasType =
          edgeCanvas

      }

    }

  }

  //============================================================================================
  // CARDINAL MARKERS
  //

  trait CardinalMarker[N <: Nat] extends ViewerMarker[N] {

    def isExtrudable : Boolean
    var cardinalAddress : Option[CardinalAddress[S[N]]] = None

  }

  trait NeutralMarker[N <: Nat] extends CardinalMarker[N] {

    def element : Option[A[N]]

    def isExtrudable : Boolean = 
      cardinalAddress match {
        case Some(_ >> Nil) => true
        case _ => false
      }

  }

  trait PolarizedMarker[N <: Nat] extends CardinalMarker[N] {

    def isExtrudable : Boolean = false

  }

  def createNeutralMarker[N <: Nat](n: N)(
    opt: Option[A[N]], 
    addr: CardinalAddress[S[N]],
    isExternal: Boolean,
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : NeutralMarkerType[N]

  def createPositiveMarker[N <: Nat](n: N)(
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : PolarizedMarkerType[N]

  def createNegativeMarker[N <: Nat](n: N)(
    objCanvas: CanvasType, 
    edgeCanvas: CanvasType
  ) : PolarizedMarkerType[N]

  //============================================================================================
  // INITIALIZATION
  //

  def initializeEditor[N <: Nat](cardinal : Cardinal[OptA, N]) : EditorState.EditorStateAux[N] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = Cardinal[OptA, N] => EditorState.EditorStateAux[N]

      def caseZero : Out[_0] = {
        case Cardinal(_, Pt(nst)) => 
          EditorState(nst)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case Cardinal(tl, hd) => 
          EditorState(initializeEditor(tl), hd)
      }

    })(cardinal.length.pred)(cardinal)

  //============================================================================================
  // EXTRUSION GARBAGE
  //

  def extendedState : EditorState = {

    val curState = editorState

    type D = curState.Dim  
    implicit val d = curState.dim

    val nextCard : CardinalNesting[Option[A[S[D]]], S[D]] = 
      curState.cardinal.head map {
        case nst => Nesting.extendNesting(nst)(_ => None)
      }

    EditorState[D](curState, nextCard)

  }

  def extend : Unit = {
    editorState = extendedState
    render
  }

  def extrudeSelection : Unit = {

    val curState = editorState

    type D = curState.Dim
    implicit val d = curState.dim

    selection match {
      case None => ()
      case Some(sel) =>
        if (sel.root.isExtrudable) {

          val selectionDim : Int = natToInt(sel.dim)
          val curStateDim : Int = natToInt(curState.dim)

          val (extrusionState, fillerEdgeCanvas) = 
            if (selectionDim == curStateDim) {
              val newState = extendedState
              (newState, newState.nextCanvas)
            } else if (selectionDim == (curStateDim - 1)) {
              (curState, curState.nextCanvas)
            } else (curState, curState.canvases(selectionDim + 2))

          val targetCanvas = extrusionState.canvases(natToInt(sel.dim))
          val fillerCanvas = extrusionState.canvases(natToInt(sel.dim) + 1)

          println("Have canvases")

          for {
            diff <- fromOpt(Lte.diffOpt(sel.dim, extrusionState.dim))
            ca <- fromOpt(sel.root.cardinalAddress)
            mk0 = createNeutralMarker(sel.dim)(None, ca, false, targetCanvas, fillerCanvas)
            mk1 = createNeutralMarker(S(sel.dim))(None, ca >> Nil, true, fillerCanvas, fillerEdgeCanvas)
            _ = mk1.outgoingEdgeMarker = Some(mk0) // This should be the only change to these, no?
            _ = println("About to perform extrusion")
            newCardinal <- Cardinal.extrudeSelection(extrusionState.cardinal, Suite.tail(ca), mk0, mk1)(
              mk => mk.isSelected
            )(diff)
          } yield {

            println("Extrusion complete")
            println("Cardinal is: " ++ newCardinal.toString)

            val newComplex : Complex[MarkerType, extrusionState.Dim] = 
              completeToComplex(extrusionState.dim)(newCardinal, extrusionState.polarizedMarkers)

            println("Finished cardinal and complex")
            println("New complex is: " ++ newComplex.toString)

            editorState = new EditorState {

              type Dim = extrusionState.Dim
              val dim = extrusionState.dim
              val complex = newComplex
              val cardinal = newCardinal
              val polarizedMarkers = extrusionState.polarizedMarkers
              val canvases = extrusionState.canvases
              val nextCanvas = extrusionState.nextCanvas

            }

            // What's left?  Some kind of refresh.

            // If we knew the actual complex addresses of the new cells, we could calculate
            // just their face complexes instead of everyones.  We could also set the correct
            // value for the address as well as the cardinal address, which right now, I think
            // is going to get fucked.

            println("About to refresh addresses")

            editorState.refreshCardinalAddresses
            editorState.refreshComplexAddresses
            editorState.refreshFaceComplexes

            // Right, the problem is now the face complexes ...

            println("About to re-render")

            deselectAll
            render

            println("Render complete ...")
          }
        }
    }
  }

}

//   //============================================================================================
//   // EXTRUSION
//   //

//   def refresh : Unit = {
//     deselectAll
//     regernerateAddresses
//     for { mk <- complex } { mk.outgoingEdgeMarker = None }
//     connectEdges(complex)
//     connectFaces(complex)
//     render
//     setNeedsLayout(true)
//   }

//   def refreshWith[N <: Nat](c : Cardinal[N, NeutralMarker]) : Unit = {
//     cardinal = c
//     refresh
//   }

//   def extend : Unit = {

//     val theCardinal = cardinal

//     // Set up the new canvases
//     val objCanvas = nextCanvas
//     nextCanvas = new CardinalCanvas
//     thisEditor.getChildren add objCanvas

//     // Add some more markers
//     polarizedMarkers :+=
//     (new PolarizedMarker(Positive(), false, objCanvas, nextCanvas),
//       new PolarizedMarker(Negative(), true, objCanvas, nextCanvas))

//     cardinal = extendCardinal(theCardinal.value)({
//       addr => {
//         println("Extended cardinal used a marker.")
//         NeutralMarker(S(theCardinal.n))(None, true, objCanvas, nextCanvas, addr >> rootAddr(S(S(theCardinal.n))))
//       }
//     })

//   }

//   def extrudeDrop(a0 : Option[A], a1 : Option[A]) : Unit = {

//     // val theCardinal = cardinal

//     for {
//       root <- selection.headOption
//       if (root.isExtrudable)
//       rootDim = natToInt(root.dim)
//       cardDim = natToInt(cardinal.n)
//       (cardToExtrude, fillerEdgeCanvas) = (
//         if (rootDim == cardDim) {
//           extend
//           extend
//           (cardinal, nextCanvas)
//         } else if (rootDim == (cardDim - 1)) {
//           extend
//           (cardinal, nextCanvas)
//         } else if (rootDim == (cardDim - 2)) {
//           (cardinal, nextCanvas)
//         } else {
//            (cardinal, canvas(rootDim + 3))
//         }
//       )
//       mk0 = NeutralMarker(S(root.dim))(a0, false, canvas(rootDim + 1), canvas(rootDim + 2), cardinalRootAddr(S(S(root.dim))))
//       mk1 = NeutralMarker(S(S(root.dim)))(a1, true, canvas(rootDim + 2), fillerEdgeCanvas, cardinalRootAddr(S(S(S(root.dim)))))
//       _ = mk0.box.toBack
//       _ = polarizedMarkers(rootDim + 1)._1.box.toBack
//       newCard <- Cardinals.doDrop(mk0, mk1, TypeSeq.tail(root.address), cardToExtrude.value)
//     } yield {
//       refreshWith(newCard)
//     }
//   }

//   def extrudeSelection(a0 : Option[A], a1 : Option[A]) : Unit = {

//     val theCardinal = cardinal

//     for {
//       root <- selection.headOption
//       if (root.isExtrudable)
//       rootDim = natToInt(root.dim)
//       cardDim = natToInt(theCardinal.n)
//       (cardToExtrude, fillerEdgeCanvas) = (
//         if (rootDim == cardDim) {
//           // We have to extend
//           extend
//           (cardinal, nextCanvas)
//         } else if (rootDim == (cardDim - 1)) {
//           (theCardinal, nextCanvas)
//         } else {
//            (theCardinal, canvas(rootDim + 2))
//         }
//       )
//       ev <- Lte.getLte(root.dim, cardToExtrude.n)
//       mk0 = NeutralMarker(root.dim)(a0, false, root.objectCanvas, root.edgeCanvas, root.address)
//       mk1 = NeutralMarker(S(root.dim))(a1, true, root.edgeCanvas, fillerEdgeCanvas, root.address >> Nil)
//       _ = mk0.box.toBack
//       _ = polarizedMarkers(natToInt(root.dim))._1.box.toBack()
//       newCard <- Cardinals.extrudeSelection(mk0, mk1, TypeSeq.tail(root.address), cardToExtrude.value)({ 
//         mk => mk.box.isSelected
//       })(ev.value)
//     } yield {
//       refreshWith(newCard)
//       // println("Cardinal result: ")
//       // println(cardinal.value.toString)
//       // println("Complex result: ")
//       // println(complex.value.toString)
//     }

//   }

//   //============================================================================================
//   // INITIALIZATION
//   //

//   def regernerateAddresses : Unit = {
//     val theCardinal = cardinal
//     theCardinal.value.fold(new ConsFold[CardinalNesting, NeutralMarker] {

//       type Out[N <: Nat] = Unit

//       def caseZero : Out[_0] = ()
//       def caseSucc[P <: Nat](p : P, cn : CardinalNesting[P, NeutralMarker], u : Unit) : Unit = {
//         Cardinals.mapCardinalNestingWithAddr(p)(cn)({
//           case (addr, mk) => {
//             for {
//               ev <- matchNatPair(p, mk.dim)
//             } {
//               type SuccAddr[N <: Nat] = CardinalAddress[S[N]]
//               mk.address = rewriteNatIn[SuccAddr, P, mk.Dim](ev)(addr)
//             }
//           }
//         })
//       }
//     })
//   }

//   def genNestingMarkers[M <: Nat](
//     nst : Nesting[M, A],
//     pref : CardinalAddress[M],
//     addr : Address[S[M]],
//     oc : CardinalCanvas,
//     ec : CardinalCanvas
//   ) : Nesting[M, NeutralMarker] =
//     (new Nesting.NestingCaseSplit[A] {

//       type Out[N <: Nat, +U <: Nesting[N, A]] = (CardinalAddress[N], Address[S[N]]) => Nesting[N, NeutralMarker]

//       def caseObj(a : A) : Out[_0, Obj[A]] =
//         (pref, addr) => Obj(NeutralMarker(Z)(Some(a), true, oc, ec, pref >> addr))

//       def caseDot[P <: Nat](a : A, d : S[P]) : Out[S[P], Dot[P, A]] =
//         (pref, addr) => Dot(NeutralMarker(d)(Some(a), true, oc, ec, pref >> addr), d)

//       def caseBox[N <: Nat](a : A, c : Tree[N, Nesting[N, A]]) : Out[N, Box[N, A]] =
//         (pref, addr) => {

//           val marker = NeutralMarker(c.dim)(Some(a), false, oc, ec, pref >> addr)
//           val canopy = c.mapWithAddress {
//             case (dir, n) => genNestingMarkers(n, pref, dir :: addr, oc, ec)
//           }

//           Box(marker, canopy)

//         }

//     })(nst)(pref, addr)

//   // We are already doing an induction on p.  So what you should do is build the list of positive and
//   // negative markers here and store it at the end.
//   def initializeCardinal[N <: Nat](card : Cardinal[N, A]) : (Cardinal[N, NeutralMarker], CardinalCanvas) = {
//     (new NatCaseSplit {

//       type Out[N <: Nat] = Cardinal[N, A] => (Cardinal[N, NeutralMarker], CardinalCanvas)

//       def caseZero : Out[_0] = {
//         case (_ >>> hd) => {

//           val objCanvas = new CardinalCanvas
//           val edgeCanvas = new CardinalCanvas

//           thisEditor.getChildren add objCanvas

//           val cardinalData : CardinalNesting[_0, NeutralMarker] = 
//             mapCardinalTreeWithAddr(Z)(hd)({
//               case (ca, nst) => genNestingMarkers(nst, ca, rootAddr(S(Z)), objCanvas, edgeCanvas)
//             })

//           val objPolarizedMarker = new PolarizedMarker(Positive(), false, objCanvas, edgeCanvas)
//           polarizedMarkers :+= (objPolarizedMarker, objPolarizedMarker)

//           (CNil[CardinalNesting]() >>> cardinalData, edgeCanvas)

//         }
//       }

//       def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
//         case (tl >>> hd) => {

//           val (newTl, objCanvas) = initializeCardinal(tl)
//           thisEditor.getChildren add objCanvas

//           val edgeCanvas = new CardinalCanvas

//           val newHd : CardinalNesting[S[P], NeutralMarker] = 
//             mapCardinalTreeWithAddr(S(p))(hd)({
//               case (ca, nst) => genNestingMarkers(nst, ca, rootAddr(S(S(p))), objCanvas, edgeCanvas)
//             })

//           // Add a pair of markers for these guys ...
//           polarizedMarkers :+= 
//           (new PolarizedMarker(Positive(), false, objCanvas, edgeCanvas),
//             new PolarizedMarker(Negative(), true, objCanvas, edgeCanvas))

//           (newTl >>> newHd, edgeCanvas)
//         }
//       }

//     })(card.dim)(card)
//   }

//   def connectEdges[N <: Nat](cmplx : Complex[N, CardinalMarker]) : Unit = {
//     (new NatCaseSplit {

//       type Out[N <: Nat] = Complex[N, CardinalMarker] => Unit

//       def caseZero : Out[_0] = {
//         case (_ >>> hd) => ()
//       }

//       def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
//         case (tl >>> hd) => {

//           connectEdges(tl)

//           hd match {
//             case Box(_, cn) => 
//               for {
//                 dots <- Nesting.spineFromCanopy(cn) 
//                 edgeTree = tl.head.toTree
//                 matchedTree <- dots.matchWith(edgeTree)
//               } yield {
//                 println("Match succeeded in dimension " ++ natToInt(S(p)).toString)
//                 matchedTree map {
//                   case (dotMarker, edgeMarker) => {
//                     dotMarker.outgoingEdgeMarker = Some(edgeMarker)
//                   }
//                 }
//               }

//             case Dot(rm, d) => {
//               rm.outgoingEdgeMarker = Some(Nesting.baseValue(tl.head))
//             }
//           }
//         }
//       }

//     })(cmplx.dim)(cmplx)
//   }

//   def connectFaces[N <: Nat](cmplx : Complex[N, CardinalMarker]) : Unit = 
//     for {
//       cm <- comultiply(cmplx)
//       _ = println("Comultiplication okay.")
//       zc <- cmplx matchWith cm
//       _ = println("Comultiplication zip succeeded.")
//       _ = mapComplex(zc)({
//         case (lb, fc) => lb.faceComplex = Some(fc)
//       })
//     } yield ()

// }
