/**
  * Cardinal.scala - Opetopic Cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scalaz.Id._
import scalaz.Monad
import scalaz.Leibniz
import scalaz.Leibniz._
import scalaz.Applicative

import scalaz.syntax.monad._

import TypeDefs._
import Nats._

trait CardinalFunctions {

  //============================================================================================
  // TYPE EQUALITY LEMMAS
  //

  @lteElim
  def treeSeqAssoc[A, N <: Nat, M <: Nat, K <: Nat, D <: Nat](lte : Lte[K, M, D]) 
      : TreeSeq[A, N, S[M]] === TreeSeq[TreeSeq[A, S[K#Plus[N]], D], N, K] = {
    case ZeroLte(k) => refl
    case SuccLte(plte : (K0, M0, D0)) => {

      lift[Nothing, Nothing, Any, Any,
        Lambda[`+B` => Tree[B, N]],
        Tree[TreeSeq[A, S[S[N]], M0], S[N]],
        TreeSeq[TreeSeq[A, S[K0#Plus[S[N]]], D0], S[N], K0]
      ](treeSeqAssoc[A, S[N], M0, K0, D0](plte)).andThen(
        rewriteNatIn[
          Lambda[`P <: Nat` => Tree[TreeSeq[TreeSeq[A, S[P], D0], S[N], K0], N]],
          K0#Plus[S[N]], S[K0#Plus[N]]
        ](plusSuccLemma[K0, N](plte.lower))
      )

    }
  }

  @natElim
  def cardinalTreeIsSeq[A, N <: Nat](n : N) : CardinalTree[A, N] === TreeSeq[A, _0, N] = {
    case Z => refl
    case S(p: P) => {

      cardinalTreeIsSeq[Tree[A, S[P]], P](p).andThen(
        rewriteNatIn[
          Lambda[`Q <: Nat` => TreeSeq[Tree[A, S[Q]], _0, P]],
          P, P#Plus[_0]
        ](plusUnitRight(p))
      ).andThen(
        simpleSymm(
          treeSeqAssoc[A, _0, P, P, _0](Lte.lteRefl(p))
        )
      )

    }
  }

  def cardinalTreeAssoc[A, K <: Nat, N <: Nat, D <: Nat](lte : Lte[K, N, D]) 
      : CardinalTree[A, S[N]] === CardinalTree[TreeSeq[A, S[K], D], K] = {

    cardinalTreeIsSeq(S(lte.upper)).andThen(
      treeSeqAssoc[A, _0, N, K, D](lte)
    ).andThen(
      rewriteNatIn[
        Lambda[`M <: Nat` => TreeSeq[TreeSeq[A, S[M], D], _0, K]],
        K#Plus[_0], K
      ](natSymm(plusUnitRight[K](lte.lower)))
    ).andThen(
      simpleSymm(cardinalTreeIsSeq(lte.lower))
    )

  }

  //============================================================================================
  // SELECTORS
  //

  def cardinalHead[A[_ <: Nat], N <: Nat](c: Cardinal[A, N]) : CardinalNesting[A[N], N] = 
    Suite.head[Lambda[`K <: Nat` => CardinalNesting[A[K], K]], N](c)

  def cardinalTail[A[_ <: Nat], N <: Nat](c: Cardinal[A, S[N]]) : Cardinal[A, N] = 
    Suite.tail[Lambda[`K <: Nat` => CardinalNesting[A[K], K]], S[N]](c)

  //============================================================================================
  // TRAVERSALS
  //

  @natElim
  def traverseCardinalTree[G[_], A, B, N <: Nat](n: N)(ct: CardinalTree[A, N])(f: A => G[B])(implicit apG: Applicative[G]) : G[CardinalTree[B, N]] = {
    case (Z, Pt(a), f) => apG.ap(f(a))(apG.pure(Pt(_)))
    case (S(p : P), ct, f) => traverseCardinalTree[G, Tree[A, S[P]], Tree[B, S[P]], P](p)(ct)(Tree.traverse(_)(f))
  }

  def mapCardinalTree[A, B, N <: Nat](n: N)(ct: CardinalTree[A, N])(f: A => B) : CardinalTree[B, N] = 
    traverseCardinalTree[Id, A, B, N](n)(ct)(f)

  @natElim
  def traverseCardinalTreeWithAddr[G[_], A, B, N <: Nat](n: N)(ct: CardinalTree[A, N])(f: (A, CardinalAddress[N]) => G[B])(
    implicit apG: Applicative[G]
  ) : G[CardinalTree[B, N]] = {
    case (Z, Pt(a), f) => apG.ap(f(a, SNil[Address]() >> (())))(apG.pure(Pt(_)))
    case (S(p : P), ct, f) => 
      traverseCardinalTreeWithAddr[G, Tree[A, S[P]], Tree[B, S[P]], P](p)(ct)({
        case (tr, base) => Tree.traverseWithAddress(tr)({
          case (a, addr) => f(a, base >> addr)
        })
      })
  }

  def mapCardinalTreeWithAddr[A, B, N <: Nat](n: N)(ct: CardinalTree[A, N])(f: (A, CardinalAddress[N]) => B) : CardinalTree[B, N] = 
    traverseCardinalTreeWithAddr[Id, A, B, N](n)(ct)(f)

  //============================================================================================
  // SEQ LEAF
  //

  @natElim
  def seqLeaf[A, N <: Nat, K <: Nat](n : N, k : K) : TreeSeq[A, S[N], K] = {
    case (n, Z) => Leaf(S(n))
    case (n, S(p)) => Leaf(S(n))
  }

  //============================================================================================
  // COMPLETE WITH
  //

  @natElim
  def completeWith[A, N <: Nat](n : N)(ct: CardinalTree[A, N], a : A) : Tree[A, N] = {
    case (Z, ct, a) => ct
    case (S(p : P), ct, a) => Node(a, completeWith(p)(ct, Leaf(S(p)))) 
  }

  def toShell[A, N <: Nat](n : N)(ct : CardinalTree[A, S[N]]) : Tree[Tree[A, S[N]], N] = 
    completeWith[Tree[A, S[N]], N](n)(ct, Leaf(S(n)))

  //============================================================================================
  // CARDINAL ADDRESS COMPlETE
  //

  // Never really debugged or looked at this guy ...

  @natElim
  def cardinalAddressComplete[N <: Nat](n: N)(ca: CardinalAddress[N]) : Address[S[N]] = {
    case (Z, _ >> hd) => () :: Nil
    case (S(p), tl >> hd) => cardinalAddressComplete(p)(tl) :: hd :: Nil
  }

  //============================================================================================
  // TO COMPLEX
  //

  @natElim
  def completeToComplex[A[_ <: Nat], B[K <: Nat] <: A[K], C[K <: Nat] <: A[K], N <: Nat](n: N)(
    c: Cardinal[B, N], ps: PolaritySuite[C, N]
  ) : Complex[A, N] = {
    case (Z, Cardinal(_, hd), PolaritySuite(_, (_, pa))) => Complex[A]() >> Box(pa, hd)
    case (S(p), Cardinal(tl, hd), PolaritySuite(ps, (na, pa))) => 
      completeToComplex[A, B, C, Nat](p)(tl, ps) >> Box(pa, Node(Dot(na, S(p)), toShell(p)(hd)))
  }

  //============================================================================================
  // COMPLEX TO CARDINAL
  //

  @natElim
  def complexToCardinal[A[_ <: Nat], N <: Nat](n: N)(cmplx: Complex[A, N]) : (Cardinal[A, N], Derivative[Nesting[A[S[N]], S[N]], S[N]]) = {
    case (Z, Complex(_, hd)) => (Cardinal[A]() >> Pt(hd) , (Pt(Leaf(__1)), Nil))
    case (S(p : P), Complex(tl, hd)) => {

      // This is the kind of thing which still needs some cleanup ....

      type INst[K <: Nat] = Nesting[A[K], K]

      val (newTl, deriv) = complexToCardinal(p)(tl)

      val nextCardinalNesting =
        mapCardinalTree[INst[P], Tree[Nesting[A[S[P]], S[P]], S[P]], P](p)(cardinalHead(newTl))(
          nst => Node(hd, deriv._1)
        )

      (newTl >> nextCardinalNesting, (Tree.const(Nesting.toTree(Complex.complexHead(tl)), Leaf(S(S(p)))), Nil))
    }
  }

  //============================================================================================
  // POKE
  //

  @natElim
  def poke[A, N <: Nat](n: N)(ct : CardinalTree[A, N], ca : CardinalAddress[N]) : ShapeM[(A, CardinalDerivative[A, N])] = {
    case (Z, Pt(a), _) => succeed(a, ())
    case (S(p), ct, tl >> hd) =>
      for {
        res <- poke(p)(ct, tl) 
        (tr, deriv) = res
        plc <- Tree.seekTo(tr, hd)
        res <- (
          plc._1 match {
            case Leaf(_) => fail("Poke failed by landing on a leaf")
            case Node(a, sh) => succeed(a, (deriv, (sh, plc._2)))
          }
        )
      } yield res

  }

  //============================================================================================
  // TAIL WITH DERIVATIVE
  //

  def tailWithDerivative[A, K <: Nat, N <: Nat, D <: Nat](ct : CardinalTree[A, S[N]], ca : CardinalAddress[K])(
    implicit lte : Lte[K, N, D]
  ) : ShapeM[(TreeSeq[A, S[K], D], CardinalDerivative[TreeSeq[A, S[K], D], K])] =
    poke(lte.lower)(cardinalTreeAssoc(lte)(ct), ca)

  //============================================================================================
  // PLUG CARDINAL
  //

  @natElim
  def plugCardinal[A, N <: Nat](n: N)(cd: CardinalDerivative[A, N], a: A) : CardinalTree[A, N] = {
    case (Z, _, a) => Pt(a)
    case (S(p), (cd, deriv), a) => 
      plugCardinal(p)(cd, Zipper.plug(S(p))(deriv, a))
  }

  def getSelection[A, N <: Nat](n: N)(cn: CardinalNesting[A, N], ca: CardinalAddress[N])(sel: A => Boolean) : ShapeM[Tree[Nesting[A, N], N]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (CardinalNesting[A, N], CardinalAddress[N]) => ShapeM[Tree[Nesting[A, N], N]]

      def caseZero : Out[_0] = {
        case (Pt(nst), ca) =>
          if (sel(Nesting.baseValue(nst))) {
            Monad[ShapeM].pure(Pt(nst))
          } else fail(new ShapeError)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (cn, tl >> hd) =>
          for {
            pr0 <- poke(p)(cn, tl)
            (tr, deriv) = pr0
            pr1 <- Tree.seekTo(tr, hd)
          } yield Tree.takeWhile(pr1._1)(nst => sel(Nesting.baseValue(nst)))
      }

    })(n)(cn, ca)

  def extrudeAt[A, B, N <: Nat](n: N)(cn: CardinalNesting[A, N], ca: CardinalAddress[N], msk: Tree[B, N], a: A) : ShapeM[CardinalNesting[A, N]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (CardinalNesting[A, N], CardinalAddress[N], Tree[B, N]) => ShapeM[CardinalNesting[A, N]]

      def caseZero : Out[_0] = {
        case (cn, ca, msk) => Monad[ShapeM].pure(Pt(Box(a, cn)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (cn, tl >> hd, msk) =>
          for {
            pr0 <- poke(p)(cn, tl)
            (tr, deriv) = pr0
            zp <- Tree.seekTo(tr, hd)
            cut <- Tree.exciseWithMask(zp._1, msk)
            (cn, cutSh) = cut
          } yield {
            plugCardinal(p)(deriv, Zipper.close(S(p))(zp._2, Node(Box(a, cn), cutSh)))
          }
      }

    })(n)(cn, ca, msk)

  def extrudeFillerAt[A, B, N <: Nat](n: N)(cn: CardinalNesting[A, S[N]], ca: CardinalAddress[N], msk: Tree[B, N], a: A) : ShapeM[CardinalNesting[A, S[N]]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (CardinalNesting[A, S[N]], CardinalAddress[N], Tree[B, N]) => ShapeM[CardinalNesting[A, S[N]]]

      def caseZero : Out[_0] = {
        case (cn, ca, msk) => Monad[ShapeM].pure(Pt(Node(Dot(a, __1), cn)))
      }

      def caseSucc[P <: Nat](p : P) : (CardinalNestingDblSucc[A, P], CardinalAddress[S[P]], Tree[B, S[P]]) => ShapeM[CardinalNestingDblSucc[A, P]] = {
        case (cn, tl >> hd, msk) =>
          for {
            pr0 <- poke(p)(cn, tl)
            (tr, deriv) = pr0
            zp <- Tree.seekTo(tr, hd)
            cut <- Tree.exciseWithMask(zp._1, msk)
            (cn, cutSh) = cut
          } yield {
            plugCardinal(p)(deriv, Zipper.close(S(p))(zp._2, Node(Node(Dot(a, S(S(p))), cn), cutSh)))
          }
      }

    })(n)(cn, ca, msk)

  def extrudeLeafAt[A, B, K <: Nat, N <: Nat, D <: Nat](cn: CardinalNesting[A, N], ca: CardinalAddress[K], msk: Tree[B, K])(
    implicit lte : Lte[S[S[K]], N, D]
  ) : ShapeM[CardinalNesting[A, N]] =
    (new NatCaseSplitWithOne { sp =>

      type Out[N <: Nat] = (Lte[S[S[K]], N, D], CardinalNesting[A, N], CardinalAddress[K]) => ShapeM[CardinalNesting[A, N]]

      def caseZero : Out[_0] = throw new IllegalArgumentException("Unreachable case")
      def caseOne : Out[_1] = throw new IllegalArgumentException("Unreachable case")

      def caseDblSucc[P <: Nat](p : P) : (Lte[S[S[K]], S[S[P]], D], CardinalNestingDblSucc[A, P], CardinalAddress[K]) => ShapeM[CardinalNestingDblSucc[A, P]] = {
        case (SuccLte(SuccLte(plte)), cn, ca) => (new LteCaseSplit {

          type Out[K <: Nat, N <: Nat, D <: Nat] = (CardinalNestingDblSucc[A, N], CardinalAddress[K], Tree[B, K]) => ShapeM[CardinalNestingDblSucc[A, N]]

          def caseZero[N <: Nat](n : N) : Out[_0, N, N] = {
            case (cn, ca, msk) => {
              for {
                pr <- tailWithDerivative[Nesting[A, S[S[N]]], _0, S[N], S[N]](cn, ca)(ZeroLte(S(n)))
              } yield {
                symm[Nothing, Any, CardinalNestingDblSucc[A, N], TreeSeqDblSucc[Nesting[A, S[S[N]]], _0, N]](
                  cardinalTreeAssoc(SuccLte(ZeroLte(n)))
                )(
                  plugCardinal(__0)(pr._2, Node(seqLeaf(__1, n), Pt(pr._1)))
                )
              }
            }
          }


          def caseSucc[K <: Nat, N <: Nat, D <: Nat](plte : Lte[K, N, D]) : Out[S[K], S[N], D] = {
            case (cn, tl >> hd, msk) => {

              val k = plte.lower
              val ev : Lte[K, S[S[N]], S[S[D]]] = Lte.lteSucc(Lte.lteSucc(plte))

              for {
                pr0 <- tailWithDerivative[Nesting[A, S[S[S[N]]]], K, S[S[N]], S[S[D]]](cn, tl)(ev)
                pr1 <- Tree.seekTo(pr0._1, hd)
                pr2 <- Tree.exciseWithMask(pr1._1, msk)
              } yield {
                symm[Nothing, Any, CardinalNestingTrplSucc[A, N], CardinalTree[TreeSeq[Nesting[A, S[S[S[N]]]], S[K], S[S[D]]], K]](
                  cardinalTreeAssoc[Nesting[A, S[S[S[N]]]], K, S[S[N]], S[S[D]]](ev)
                )(
                  plugCardinal(k)(pr0._2,
                    Zipper.close(S(k))(pr1._2, Node(Node(seqLeaf(S(S(k)), plte.diff), pr2._1), pr2._2))
                  )
                )
              }
            }
          }

        })(plte)(cn, ca, msk)
      }

    })(lte.upper)(lte, cn, ca)

  def extrudeLoopAt[A, N <: Nat](n: N)(cn: CardinalNesting[A, S[N]], ca: CardinalAddress[N], a: A) : ShapeM[CardinalNesting[A, S[N]]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (CardinalNesting[A, S[N]], CardinalAddress[N]) => ShapeM[CardinalNesting[A, S[N]]]

      def caseZero : Out[_0] = {
        case (cn, ca) => Monad[ShapeM].pure(Pt(Node(Box(a, Leaf(__1)), cn)))
      }

      def caseSucc[P <: Nat](p : P) : (CardinalNestingDblSucc[A, P], CardinalAddress[S[P]]) => ShapeM[CardinalNestingDblSucc[A, P]] = {
        case (cn, ca) =>
          for {
            pr <- poke(S(p))(cn, ca) 
          } yield {
            plugCardinal[Tree[Nesting[A, S[S[P]]], S[S[P]]], S[P]](S(p))(pr._2,
              Node(Box(a, Leaf(S(S(p)))), Node(pr._1, Tree.const(pr._2._2._1, Leaf(S(p)))))
            )
          }
      }

    })(n)(cn, ca)

  def extrudeDropAt[A, N <: Nat](n: N)(cn: CardinalNestingDblSucc[A, N], ca: CardinalAddress[N], a: A) : ShapeM[CardinalNestingDblSucc[A, N]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (CardinalNestingDblSucc[A, N], CardinalAddress[N]) => ShapeM[CardinalNestingDblSucc[A, N]]

      def caseZero : Out[_0] = {
        case (cn, ca) => Monad[ShapeM].pure(Pt(Node(Node(Dot(a, __2), Leaf(__1)), cn)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (cn, ca) =>
          for {
            pr <- poke(S(p))(cn, ca) 
          } yield {
            plugCardinal[Tree[Tree[Nesting[A, S[S[S[P]]]], S[S[S[P]]]], S[S[P]]], S[P]](S(p))(pr._2,
              Node(Node(Dot(a, S(S(S(p)))), Leaf(S(S(p)))), Node(pr._1, Tree.const(pr._2._2._1, Leaf(S(p)))))
            )
          }
      }

    })(n)(cn, ca)

  def extrudeDropLeafAt[A, K <: Nat, N <: Nat, D <: Nat](cn: CardinalNesting[A, N], ca: CardinalAddress[K])(
    implicit lte: Lte[S[S[S[K]]], N, D]
  ) : ShapeM[CardinalNesting[A, N]] =
    (new NatCaseSplitWithTwo {

      type Out[N <: Nat] = (Lte[S[S[S[K]]], N, D], CardinalNesting[A, N], CardinalAddress[K]) => ShapeM[CardinalNesting[A, N]]

      def caseZero : Out[_0] = throw new IllegalArgumentException("Unreachable case")
      def caseOne : Out[_1] = throw new IllegalArgumentException("Unreachable case")
      def caseTwo : Out[_2] = throw new IllegalArgumentException("Unreachable case")

      def caseTrplSucc[P <: Nat](p: P) : (Lte[S[S[S[K]]], S[S[S[P]]], D], CardinalNestingTrplSucc[A, P], CardinalAddress[K]) => ShapeM[CardinalNestingTrplSucc[A, P]] = {
        case (SuccLte(SuccLte(SuccLte(lte))), cn, ca) =>
          (new LteCaseSplit {

            type Out[K <: Nat, N <: Nat, D <: Nat] = (CardinalNestingTrplSucc[A, N], CardinalAddress[K]) => ShapeM[CardinalNestingTrplSucc[A, N]]

            def caseZero[N <: Nat](n: N) : Out[_0, N, N] = {
              case (cn, ca) =>
                for {
                  pr <- tailWithDerivative[Nesting[A, S[S[S[N]]]], _0, S[S[N]], S[S[N]]](cn, ca)(ZeroLte(S(S(n))))
                } yield {
                  symm[Nothing, Any, CardinalNestingTrplSucc[A, N], TreeSeqTrplSucc[Nesting[A, S[S[S[N]]]], _0, N]](
                    cardinalTreeAssoc[Nesting[A, S[S[S[N]]]], _2, S[S[N]], N](SuccLte(SuccLte(ZeroLte(n))))
                  )(
                    plugCardinal(__0)(pr._2, Node(Node(seqLeaf(__2, n), Leaf(__1)), Pt(pr._1)))
                  )
                }
            }

            def caseSucc[K <: Nat, N <: Nat, D <: Nat](plte: Lte[K, N, D]) : Out[S[K], S[N], D] = {
              case (cn, ca) => {

                import Lte._

                val k : K = plte.lower
                val n : N = plte.upper
                val d : D = plte.diff

                val ev : Lte[S[K], S[S[S[N]]], S[S[D]]] = lteSucc(lteSucc(SuccLte(plte)))

                for {
                  pr <- tailWithDerivative[Nesting[A, S[S[S[S[N]]]]], S[K], S[S[S[N]]], S[S[D]]](cn, ca)(ev)
                } yield {
                  symm[Nothing, Any, CardinalNestingQuadSucc[A, N], CardinalTree[TreeSeq[Nesting[A, S[S[S[S[N]]]]], S[S[K]], S[S[D]]], S[K]]](
                    cardinalTreeAssoc[Nesting[A, S[S[S[S[N]]]]], S[K], S[S[S[N]]], S[S[D]]](ev)
                  )(
                    plugCardinal(S(k))(pr._2, Node(Node(seqLeaf(S(S(S(k))), d), Leaf(S(S(k)))), Node(pr._1, Tree.const(pr._2._2._1, Leaf(S(k))))))
                  )
                }
              }
            }

          })(lte)(cn, ca)
      }

    })(lte.upper)(lte, cn, ca)

  def sproutAt[A, N <: Nat](n: N)(cn: CardinalNesting[A, N], ca: CardinalAddress[S[N]], a: A) : ShapeM[CardinalNesting[A, N]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (CardinalNesting[A, N], CardinalAddress[S[N]]) => ShapeM[CardinalNesting[A, N]]

      def caseZero : Out[_0] = {
        case (Pt(nst), (tl >> hd)) => 
          for {
            zp <- Nesting.seekNesting(__0)((nst, Nil), hd)
            res <- (
              zp._1 match {
                case Obj(a1) => succeed(Pt(Nesting.closeNesting(__0)(zp._2, Box(a1, Pt(Obj(a))))))
                case Box(a1, cn) => fail(new ShapeError("Cannot sprout from box"))
              }
            )
          } yield res
      }

      def caseSucc[P <: Nat](p: P) : Out[S[P]] = {
        case (cn, tl >> hd) => 
          for {
            pr0 <- poke(S(p))(cn, tl) 
            (nst, deriv) = pr0
            pr1 <- Nesting.seekNesting(S(p))((nst, Nil), hd)
            res <- (
              pr1 match {
                case (Dot(a1, sp), cs) => {
                  val sh = 
                    (cs.headOption map ((c : (_, Derivative[_, S[P]])) => Tree.const(c._2._1, Leaf(S(p))))).
                      getOrElse(Tree.const(deriv._2._1, Leaf(S(p))))

                  succeed(plugCardinal(S(p))(deriv, Nesting.closeNesting(S(p))(cs, Box(a1, Node(Dot(a, S(p)), sh)))))
                }
                case (Box(_, _), _) => fail(new ShapeError("Cannot sprout from box"))
              }
            )
          } yield res
      }

    })(n)(cn, ca)

  // Indeed, the address must be adjusted be the nestings which appear in the next dimension.  Thereafter, I believe
  // everything will be the same.  But the address can indeed be shortened by the filling dimension if some boxes
  // surround the main tree.

  def sproutFillerAt[A, N <: Nat](n: N)(cn: CardinalNesting[A, S[N]], ca: CardinalAddress[S[N]], a: A) : ShapeM[CardinalNesting[A, S[N]]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (CardinalNesting[A, S[N]], CardinalAddress[S[N]]) => ShapeM[CardinalNesting[A, S[N]]]

      def caseZero : Out[_0] = {
        case (cn, tl >> hd) => {
          println("In debug case")
          for {
            pr0 <- poke(__0)(cn, tl) 
            _ = println("Poke complete")
            pr1 <- Tree.seekTo(pr0._1, hd)
            _ = println("Seek complete")
            res <- (
              pr1._1 match {
                case Leaf(_) => succeed(
                  plugCardinal(__0)(pr0._2, Zipper.close(__1)(pr1._2, Node(Dot(a, __1), Pt(Leaf(__1)))))
                )
                case Node(_, _) => fail(new ShapeError("Expected a leaf"))
              }
            )
          } yield res
        }
      }

      def caseSucc[P <: Nat](p: P) : (CardinalNestingDblSucc[A, P], CardinalAddress[S[S[P]]]) => ShapeM[CardinalNestingDblSucc[A, P]] = {
        case (cn, tl >> hd) => 
          for {
            pr0 <- poke(S(p))(cn, tl) 
            pr1 <- Tree.seekTo(pr0._1, hd)
            res <- (
              pr1._1 match {
                case Leaf(_) => {
                  val sh = 
                    (pr1._2.headOption map ((c : (_, Derivative[_, S[P]])) => Tree.const(c._2._1, Leaf(S(p))))).
                      getOrElse(Tree.const(pr0._2._2._1, Leaf(S(p))))

                  succeed(
                    plugCardinal(S(p))(pr0._2, Zipper.close(S(S(p)))(pr1._2, Node(Dot(a, S(S(p))), Node(Leaf(S(S(p))), sh))))
                  )
                }
                case Node(_, _) => fail(new ShapeError("Expected a leaf"))
              }
            )
          } yield res
      }

    })(n)(cn, ca)

  def sproutLeafAt[A, K <: Nat, N <: Nat, D <: Nat](cn: CardinalNesting[A, N], ca: CardinalAddress[S[K]])(
    implicit lte: Lte[S[S[K]], N, D]
  ) : ShapeM[CardinalNesting[A, N]] = 
    (new NatCaseSplitWithOne { sp =>

      type Out[N <: Nat] = (Lte[S[S[K]], N, D], CardinalNesting[A, N], CardinalAddress[S[K]]) => ShapeM[CardinalNesting[A, N]]

      def caseZero : Out[_0] = throw new IllegalArgumentException("Unreachable case")
      def caseOne : Out[_1] = throw new IllegalArgumentException("Unreachable case")

      def caseDblSucc[P <: Nat](p : P) : (Lte[S[S[K]], S[S[P]], D], CardinalNestingDblSucc[A, P], CardinalAddress[S[K]]) 
        => ShapeM[CardinalNestingDblSucc[A, P]] = {
          case (SuccLte(SuccLte(pplte)), cn, ca) => (new LteCaseSplit {

            type Out[K <: Nat, N <: Nat, D <: Nat] = (CardinalNestingDblSucc[A, N], CardinalAddress[S[K]]) => ShapeM[CardinalNestingDblSucc[A, N]]

            def caseZero[N <: Nat](n: N) : Out[_0, N, N] = {
              case (cn, tl >> hd) => 
                for {
                  pr0 <- tailWithDerivative[Nesting[A, S[S[N]]], _0, S[N], S[N]](cn, tl)(ZeroLte(S(n)))
                  pr1 <- Tree.seekTo(pr0._1, hd)
                  res <- (
                    pr1._1 match {
                      case Leaf(_) => {
                        succeed(
                          symm[Nothing, Any, CardinalNestingDblSucc[A, N], TreeSeqDblSucc[Nesting[A, S[S[N]]], _0, N]](
                            cardinalTreeAssoc(SuccLte(ZeroLte(n)))
                          )(
                            plugCardinal(__0)(pr0._2, Zipper.close(__1)(pr1._2, Node(seqLeaf(__1, n), Pt(Leaf(__1)))))
                          )
                        )
                      }
                      case Node(_, _) => fail(new ShapeError("Expected a leaf"))
                    }
                  )
                } yield res
            }

            def caseSucc[K <: Nat, N <: Nat, D <: Nat](plte: Lte[K, N, D]) : Out[S[K], S[N], D] = {
              case (cn, tl >> hd) => {
                
                val k = plte.lower
                val ev : Lte[S[K], S[S[N]], S[D]] = SuccLte(Lte.lteSucc(plte))

                for {
                  pr0 <- tailWithDerivative[Nesting[A, S[S[S[N]]]], S[K], S[S[N]], S[D]](cn, tl)(ev)
                  pr1 <- Tree.seekTo(pr0._1, hd)
                  res <- (
                    pr1._1 match {
                      case Leaf(_) => {
                        val sh =
                          (pr1._2.headOption map ((c : (_, Derivative[_, S[K]])) =>Tree.const(c._2._1, Leaf(S(k))))).
                            getOrElse(Tree.const(pr0._2._2._1, Leaf(S(k))))

                        succeed(
                          symm[Nothing, Any, CardinalNestingTrplSucc[A, N], CardinalTree[TreeSeq[Nesting[A, S[S[S[N]]]], S[S[K]], S[D]], S[K]]](
                            cardinalTreeAssoc[Nesting[A, S[S[S[N]]]], S[K], S[S[N]], S[D]](ev)
                          )(
                            plugCardinal(S(k))(pr0._2, Zipper.close(S(S(k)))(pr1._2, Node(seqLeaf(S(S(k)), plte.diff), Node(Leaf(S(S(k))), sh))))
                          )
                        )
                      }
                      case Node(_, _) => fail(new ShapeError("Expected a leaf, damnit!"))
                    }
                  )
                } yield res
              }
            }

          })(pplte)(cn, ca)
        }

    })(lte.upper)(lte, cn, ca)

  //============================================================================================
  // DIMENSION FLAGS
  //

  sealed trait CardinalDimFlag[N <: Nat, K <: Nat] {
    def succ : CardinalDimFlag[S[N], S[K]]
    def dispatch(disp : DimDispatcher[K]) : disp.Out[N]
  }

  case class DimEq[K <: Nat](k : K) extends CardinalDimFlag[K, K] {
    def succ : CardinalDimFlag[S[K], S[K]] = DimEq(S(k))
    def dispatch(disp : DimDispatcher[K]) : disp.Out[K] =
      disp.caseEq(k)
  }

  case class DimSucc[K <: Nat](k : K) extends CardinalDimFlag[S[K], K] {
    def succ : CardinalDimFlag[S[S[K]], S[K]] = DimSucc(S(k))
    def dispatch(disp : DimDispatcher[K]) : disp.Out[S[K]] =
      disp.caseSucc(k)
  }

  case class DimLt[N <: Nat, K <: Nat, D <: Nat](slte : Lte[S[N], K, D]) extends CardinalDimFlag[N, K] {
    def succ : CardinalDimFlag[S[N], S[K]] = DimLt(SuccLte(slte))
    def dispatch(disp : DimDispatcher[K]) : disp.Out[N] =
      disp.caseLt(slte)
  }

  case class DimDblSucc[N <: Nat, K <: Nat, D <: Nat](sslte : Lte[S[S[K]], N, D]) extends CardinalDimFlag[N, K] {
    def succ : CardinalDimFlag[S[N], S[K]] = DimDblSucc(SuccLte(sslte))
    def dispatch(disp : DimDispatcher[K]) : disp.Out[N] =
      disp.caseDblSucc(sslte)
  }

  trait DimDispatcher[K <: Nat] {

    type Out[N <: Nat]

    def caseEq(k : K) : Out[K]
    def caseLt[N <: Nat, D <: Nat](slte : Lte[S[N], K, D]) : Out[N]
    def caseSucc(k : K) : Out[S[K]]
    def caseDblSucc[N <: Nat, D <: Nat](sslte : Lte[S[S[K]], N, D]) : Out[N]

    def apply[N <: Nat](flag : CardinalDimFlag[N, K]) : Out[N] =
      flag.dispatch(this)

  }

  def getFlag[N <: Nat, K <: Nat](n : N, k : K) : CardinalDimFlag[N, K] = 
    (new NatCaseSplit0 {

      type Out[K <: Nat] = CardinalDimFlag[N, K]

      def caseZero : Out[_0] = 
        (new NatCaseSplitWithOne {

          type Out[N <: Nat] = CardinalDimFlag[N, _0]

          def caseZero : Out[_0] = DimEq(Z)
          def caseOne : Out[_1] = DimSucc(Z)
          def caseDblSucc[P <: Nat](p : P) : Out[S[S[P]]] = 
            DimDblSucc(SuccLte(SuccLte(ZeroLte(p))))

        })(n)

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = 
        (new NatCaseSplit0 {

          type Out[N <: Nat] = CardinalDimFlag[N, S[P]]

          def caseZero : Out[_0] = DimLt(SuccLte(ZeroLte(p)))
          def caseSucc[PN <: Nat](pn : PN) : Out[S[PN]] = getFlag[PN, P](pn, p).succ
 
        })(n)

    })(k)

  //============================================================================================
  // EXTRUDE DISPATCH
  //

  def extrudeDispatch[A[_ <: Nat], B, K <: Nat, N <: Nat](
    cn : CardinalNesting[A[N], N],
    ca : CardinalAddress[K],
    a0 : A[K], a1 : A[S[K]],
    msk : Tree[B, K],
    flag : CardinalDimFlag[N, K]
  ) : ShapeM[CardinalNesting[A[N], N]] =
    (new DimDispatcher[K] {

      type Out[N <: Nat] = (CardinalNesting[A[N], N], CardinalAddress[K], Tree[B, K]) => ShapeM[CardinalNesting[A[N], N]]

      def caseLt[N <: Nat, D <: Nat](slte : Lte[S[N], K, D]) : Out[N] = { 
        case (cn, ca, msk) => Monad[ShapeM].pure(cn) 
      }

      def caseEq(k : K) : Out[K] = { 
        case (cn, ca, msk) => extrudeAt(k)(cn, ca, msk, a0) 
      }

      def caseSucc(k : K) : Out[S[K]] = { 
        case (cn, ca, msk) => extrudeFillerAt(k)(cn, ca, msk, a1) 
      }

      def caseDblSucc[N <: Nat, D <: Nat](sslte : Lte[S[S[K]], N, D]) : Out[N] = { 
        case (cn, ca, msk) => extrudeLeafAt(cn, ca, msk)(sslte) 
      }

    })(flag)(cn, ca, msk)

  //============================================================================================
  // DROP DISPATCH
  //

  def dropDispatch[A[_ <: Nat], N <: Nat, K <: Nat](
    cn : CardinalNesting[A[N], N],
    ca : CardinalAddress[K],
    a0 : A[S[K]], a1 : A[S[S[K]]],
    flag : CardinalDimFlag[N, S[K]]
  ) : ShapeM[CardinalNesting[A[N], N]] = 
    (new DimDispatcher[S[K]] {

      type Out[N <: Nat] = CardinalNesting[A[N], N] => ShapeM[CardinalNesting[A[N], N]]

      def caseLt[N <: Nat, D <: Nat](slte : Lte[S[N], S[K], D]) : Out[N] = { 
        cn => Monad[ShapeM].pure(cn) 
      }

      def caseEq(sk : S[K]) : Out[S[K]] = { 
        cn => extrudeLoopAt(sk.pred)(cn, ca, a0) 
      }

      def caseSucc(sk : S[K]) : CardinalNestingDblSucc[A[S[S[K]]], K] => ShapeM[CardinalNestingDblSucc[A[S[S[K]]], K]] = {
        cn => extrudeDropAt(sk.pred)(cn, ca, a1)
      }

      def caseDblSucc[N <: Nat, D <: Nat](sslte : Lte[S[S[S[K]]], N, D]) : Out[N] = {
        cn => extrudeDropLeafAt(cn, ca)(sslte)
      }

    })(flag)(cn)

  //============================================================================================
  // SPROUT DISPATCH
  //

  def sproutDispatch[A[_ <: Nat], N <: Nat, K <: Nat](
    cn: CardinalNesting[A[N], N],
    ca: CardinalAddress[S[K]],
    a0: A[K], a1: A[S[K]],
    flag: CardinalDimFlag[N, K]
  ) : ShapeM[CardinalNesting[A[N], N]] = 
    (new DimDispatcher[K] {

      type Out[N <: Nat] = (CardinalNesting[A[N], N], CardinalAddress[S[K]]) => ShapeM[CardinalNesting[A[N], N]]

      def caseLt[N <: Nat, D <: Nat](slte : Lte[S[N], K, D]) : Out[N] = { 
        case (cn, ca) => Monad[ShapeM].pure(cn)
      }

      def caseEq(k : K) : Out[K] = { 
        case (cn, ca) => sproutAt(k)(cn, ca, a0)
      }

      def caseSucc(k : K) : Out[S[K]] = { 
        case (cn, ca) => sproutFillerAt(k)(cn, ca, a1)
      }

      def caseDblSucc[N <: Nat, D <: Nat](sslte : Lte[S[S[K]], N, D]) : Out[N] = { 
        case (cn, ca) => sproutLeafAt(cn, ca)(sslte)
      }

    })(flag)(cn, ca)

  //============================================================================================
  // FULL EXTRUSTION ROUTINES
  //

  def extrudeSelection[A[_ <: Nat], N <: Nat, K <: Nat](
    c: Cardinal[A, N], ca: CardinalAddress[K],
    a0 : A[K], a1 : A[S[K]])(p: A[K] => Boolean
  )(implicit diff: Lte.Diff[K, N]) : ShapeM[Cardinal[A, N]] = {

    type KNesting[K <: Nat] = CardinalNesting[A[K], K]

    val lte = diff.lte

    for {
      msk <- getSelection(lte.lower)(Suite.getAt[KNesting, K, N, diff.D](c)(lte), ca)(p)
      res <- Suite.traverse[ShapeM, KNesting, KNesting, S[N]](c)(new IndexedTraverse[ShapeM, KNesting, KNesting] {
        def apply[M <: Nat](m: M)(cn: KNesting[M]) : ShapeM[KNesting[M]] = {
          extrudeDispatch[A, Nesting[A[K], K], K, M](cn, ca, a0, a1, msk, getFlag(m, lte.lower))
        }
      })
    } yield res
  }

  def dropAtAddress[A[_ <: Nat], N <: Nat, K <: Nat](
    c: Cardinal[A, N], ca: CardinalAddress[K],
    a0: A[S[K]], a1: A[S[S[K]]]
  )(implicit diff: Lte.Diff[K, N]) : ShapeM[Cardinal[A, N]] = {
    type KNesting[K <: Nat] = CardinalNesting[A[K], K]
    Suite.traverse[ShapeM, KNesting, KNesting, S[N]](c)(new IndexedTraverse[ShapeM, KNesting, KNesting] {
      def apply[M <: Nat](m: M)(cn: KNesting[M]) : ShapeM[KNesting[M]] = {
        dropDispatch(cn, ca, a0, a1, getFlag(m, S(diff.lte.lower)))
      }
    })
  }

  def sproutAtAddress[A[_ <: Nat], N <: Nat, K <: Nat](
    c: Cardinal[A, N], ca: CardinalAddress[S[K]],
    a0: A[K], a1: A[S[K]]
  )(implicit diff: Lte.Diff[K, N]) : ShapeM[Cardinal[A, N]] = {
    type KNesting[K <: Nat] = CardinalNesting[A[K], K]

    Suite.traverse[ShapeM, KNesting, KNesting, S[N]](c)(new IndexedTraverse[ShapeM, KNesting, KNesting] {
      def apply[M <: Nat](m: M)(cn: KNesting[M]) : ShapeM[KNesting[M]] = {
        println("Dispatching in dimension " ++ natToInt(m).toString)
        val res = sproutDispatch(cn, ca, a0, a1, getFlag(m, diff.lte.lower))
        println("Result: " ++ res.toString)
        res
      }
    })
  }

}

object Cardinal extends CardinalFunctions {

  def apply[A[_ <: Nat]]() : Suite[({ type L[K <: Nat] = CardinalNesting[A[K], K] })#L, _0] = 
    SNil[({ type L[K <: Nat] = CardinalNesting[A[K], K] })#L]()

  def unapply[A[_ <: Nat], N <: Nat](suite : Suite[({ type L[K <: Nat] = CardinalNesting[A[K], K] })#L, S[N]])
      : Option[(Suite[({ type L[K <: Nat] = CardinalNesting[A[K], K] })#L, N], CardinalNesting[A[N], N])] = {
    type IdxdNesting[K <: Nat] = CardinalNesting[A[K], K]
    Some((Suite.tail[IdxdNesting, N](suite), Suite.head[IdxdNesting, N](suite)))
  }

}
