/**
  * Cardinal.scala - Opetopic Cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.language.implicitConversions

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

  def treeSeqAssoc[A, N <: Nat, M <: Nat, K <: Nat, D <: Nat](implicit lte : Lte[K, M, D]) 
      : TreeSeq[A, N, S[M]] === TreeSeq[TreeSeq[A, S[K#Plus[N]], D], N, K] = 
    (new LteCaseSplit {

      type Out[K <: Nat, M <: Nat, D <: Nat] = 
        TreeSeq[A, N, S[M]] === TreeSeq[TreeSeq[A, S[K#Plus[N]], D], N, K]

      def caseZero[K <: Nat](k : K) : TreeSeq[A, N, S[K]] === TreeSeq[TreeSeq[A, S[N], K], N, _0] = 
        refl

      def caseSucc[K <: Nat, M <: Nat, D <: Nat](plte : Lte[K, M, D])
          : TreeSeq[A, N, S[S[M]]] === TreeSeq[TreeSeq[A, S[S[K]#Plus[N]], D], N, S[K]] = {

        val step1 : Tree[Tree[TreeSeq[A, S[S[N]], M], S[N]], N] ===
                    Tree[TreeSeq[TreeSeq[A, S[K#Plus[S[N]]], D], S[N], K], N] = 
                      lift[Nothing, Nothing, Any, Any, 
                        ({ type L[+B] = Tree[B, N] })#L, 
                        Tree[TreeSeq[A, S[S[N]], M], S[N]], 
                        TreeSeq[TreeSeq[A, S[K#Plus[S[N]]], D], S[N], K]
                      ](treeSeqAssoc[A, S[N], M, K, D](plte))

        val step2 : Tree[TreeSeq[TreeSeq[A, S[K#Plus[S[N]]], D], S[N], K], N] === 
                    Tree[TreeSeq[TreeSeq[A, S[S[K#Plus[N]]], D], S[N], K], N] =
                      lift[Nothing, Nothing, Nat, Any,
                        ({ type L[P <: Nat] = Tree[TreeSeq[TreeSeq[A, S[P], D], S[N], K], N] })#L,
                        K#Plus[S[N]], S[K#Plus[N]]
                      ](plusSuccLemma[K, N](plte.lower))

        step1.andThen(step2)

      }

    })(lte)

  def cardinalTreeIsSeq[A, N <: Nat](n : N) : CardinalTree[A, N] === TreeSeq[A, _0, N] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = CardinalTree[A, N] === TreeSeq[A, _0, N]

      def caseZero : CardinalTree[A, _0] === TreeSeq[A, _0, _0] = refl

      def caseSucc[P <: Nat](p : P) : CardinalTree[A, S[P]] === TreeSeq[A, _0, S[P]] = {

        val step1 : CardinalTree[Tree[A, S[P]], P] === TreeSeq[Tree[A, S[P]], _0, P] = 
          cardinalTreeIsSeq[Tree[A, S[P]], P](p)

        val step2 : TreeSeq[Tree[A, S[P]], _0, P] === TreeSeq[TreeSeq[A, S[P#Plus[_0]], _0], _0, P] = 
          lift[Nothing, Nothing, Nat, Any,
            ({ type L[Q <: Nat] = TreeSeq[Tree[A, S[Q]], _0, P] })#L,
            P, P#Plus[_0]
          ](plusUnitRight(p))

        val step3 : TreeSeq[TreeSeq[A, S[P#Plus[_0]], _0], _0, P] === Tree[TreeSeq[A, _1, P], _0] = 
          symm[Nothing, Any, TreeSeq[A, _0, S[P]], TreeSeq[TreeSeq[A, S[P#Plus[_0]], _0], _0, P]](
            treeSeqAssoc[A, _0, P, P, _0](Lte.lteRefl(p))
          )

        step1.andThen(step2).andThen(step3)

      }

    })(n)

  def cardinalTreeAssoc[A, K <: Nat, N <: Nat, D <: Nat](implicit lte : Lte[K, N, D]) : CardinalTree[A, S[N]] === CardinalTree[TreeSeq[A, S[K], D], K] = {

    val step1 : CardinalTree[A, S[N]] === TreeSeq[A, _0, S[N]] = 
      cardinalTreeIsSeq(S(lte.upper))

    val step2 : TreeSeq[A, _0, S[N]] === TreeSeq[TreeSeq[A, S[K#Plus[_0]], D], _0, K] =
      treeSeqAssoc

    val step3 : TreeSeq[TreeSeq[A, S[K#Plus[_0]], D], _0, K] === TreeSeq[TreeSeq[A, S[K], D], _0, K] = 
      lift[
        Nothing, Nothing, Nat, Any,
        ({ type L[X <: Nat] = TreeSeq[TreeSeq[A, S[X], D], _0, K] })#L,
        K#Plus[_0], K
      ](symm[Nothing, Nat, K, K#Plus[_0]](plusUnitRight[K](lte.lower)))

    val step4 : TreeSeq[TreeSeq[A, S[K], D], _0, K] === CardinalTree[TreeSeq[A, S[K], D], K] = 
      symm[Nothing, Any, 
        CardinalTree[TreeSeq[A, S[K], D], K], 
        TreeSeq[TreeSeq[A, S[K], D], _0, K]
      ](cardinalTreeIsSeq(lte.lower))

    step1.andThen(step2).andThen(step3).andThen(step4)

  }

  //============================================================================================
  // TRAVERSALS
  //

  def traverseCardinalTree[G[_], A, B, N <: Nat](n: N)(ct: CardinalTree[A, N])(f: A => G[B])(implicit apG: Applicative[G]) : G[CardinalTree[B, N]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = CardinalTree[A, N] => G[CardinalTree[B, N]]

      def caseZero : Out[_0] = {
        case Pt(a) => apG.ap(f(a))(apG.pure(Pt(_)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case ct => traverseCardinalTree[G, Tree[A, S[P]], Tree[B, S[P]], P](p)(ct)(Tree.traverse(_)(f))
      }

    })(n)(ct)

  def mapCardinalTree[A, B, N <: Nat](n: N)(ct: CardinalTree[A, N])(f: A => B) : CardinalTree[B, N] = 
    traverseCardinalTree[Id, A, B, N](n)(ct)(f)

  def traverseCardinalTreeWithAddr[G[_], A, B, N <: Nat](n: N)(ct: CardinalTree[A, N])(f: (A, CardinalAddress[N]) => G[B])(
    implicit apG: Applicative[G]
  ) : G[CardinalTree[B, N]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (CardinalTree[A, N], (A, CardinalAddress[N]) => G[B]) => G[CardinalTree[B, N]]

      def caseZero : Out[_0] = {  // Seriously, fix this notation
        case (Pt(a), f) => apG.ap(f(a, SNil[Address]() >> (())))(apG.pure(Pt(_)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (ct, f) => traverseCardinalTreeWithAddr[G, Tree[A, S[P]], Tree[B, S[P]], P](p)(ct)({
          case (tr, base) => Tree.traverseWithAddress(tr)({
            case (a, addr) => f(a, base >> addr)
          })
        })
      }

    })(n)(ct, f)

  def mapCardinalTreeWithAddr[A, B, N <: Nat](n: N)(ct: CardinalTree[A, N])(f: (A, CardinalAddress[N]) => B) : CardinalTree[B, N] = 
    traverseCardinalTreeWithAddr[Id, A, B, N](n)(ct)(f)

  //============================================================================================
  // SEQ LEAF
  //

  def seqLeaf[A, N <: Nat, K <: Nat](n : N, k : K) : TreeSeq[A, S[N], K] = 
    (new NatCaseSplit0 {

      type Out[M <: Nat] = TreeSeq[A, S[N], M]

      def caseZero : TreeSeq[A, S[N], _0] = Leaf(S(n))
      def caseSucc[P <: Nat](p : P) : TreeSeq[A, S[N], S[P]] = Leaf(S(n))

    })(k)

  //============================================================================================
  // COMPLETE WITH
  //

  def completeWith[A, N <: Nat](n : N)(ct: CardinalTree[A, N], a : A) : Tree[A, N] =
    (new NatCaseSplit0 {
      
      type Out[N <: Nat] = CardinalTree[A, N] => Tree[A, N]

      def caseZero : Out[_0] = { ct => ct }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        ct => Node(a, completeWith[Tree[A, S[P]], P](p)(ct, Leaf(S(p))))
      }

    })(n)(ct)

  def toShell[A, N <: Nat](n : N)(ct : CardinalTree[A, S[N]]) : Tree[Tree[A, S[N]], N] = 
    completeWith[Tree[A, S[N]], N](n)(ct, Leaf(S(n)))

  //============================================================================================
  // CARDINAL ADDRESS COMPlETE
  //

  def cardinalAddressComplete[N <: Nat](n: N)(ca: CardinalAddress[N]) : Address[S[N]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = CardinalAddress[N] => Address[S[N]] 

      def caseZero : Out[_0] = {
        case (_ >> hd) => () :: Nil
      }

      def caseSucc[P <: Nat](p: P) : Out[S[P]] = {
        case (tl >> hd) => this(p)(tl) :: hd :: Nil
      }

    })(n)(ca)

  //============================================================================================
  // TO COMPLEX
  //

  sealed trait Polarity[A]
  sealed trait Polarization[A] extends Polarity[A]
  case class Positive[A]() extends Polarization[A] { override def toString = "+" }
  case class Negative[A]() extends Polarization[A] { override def toString = "-" }
  case class Neutral[A](a : A) extends Polarity[A] { override def toString = a.toString }

  trait CardinalCellGenerator[D <: Nat, A[_ <: Nat], B[_ <: Nat]] {

    def positive[N <: Nat](n: N)(diff: Lte.Diff[N, D]) : B[N]
    def negative[N <: Nat](n: N)(diff: Lte.Diff[N, D]) : B[N]

    def neutral[N <: Nat](n: N)(a: A[N])(diff: Lte.Diff[N, D]) : B[N]

  }

  def completeToComplex[A[_ <: Nat], B[K <: Nat] <: A[K], C[K <: Nat] <: A[K], N <: Nat](n: N)(
    c: Cardinal[B, N], p: PolaritySuite[C, N]
  ) : Complex[A, N] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (Cardinal[B, N], PolaritySuite[C, N]) => Complex[A, N]

      def caseZero : Out[_0] = {
        case (Cardinal(_, hd), PolaritySuite(_, (_, pa))) => {
          Complex[A]() >> Box(pa, hd)
        }
      }

      def caseSucc[P <: Nat](p: P) : Out[S[P]] = {
        case (Cardinal(tl, hd), PolaritySuite(ps, (na, pa))) => {
          this(p)(tl, ps) >> Box(pa, Node(Dot(na, S(p)), toShell(p)(hd)))
        }
      }

    })(n)(c, p)

  //============================================================================================
  // POKE
  //

  def poke[A, N <: Nat](ct : CardinalTree[A, N], ca : CardinalAddress[N]) : ShapeM[(A, CardinalDerivative[A, N])] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (CardinalTree[A,  N], CardinalAddress[N]) => ShapeM[(A, CardinalDerivative[A, N])]

      def caseZero : Out[_0] = {
        case (Pt(a), _) => Monad[ShapeM].pure(a, ())
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (ct, tl >> hd) =>
          for {
            res <- poke[Tree[A, S[P]], P](ct, tl)
            (tr, deriv) = res
            plc <- Tree.seekTo(tr, hd)
            res <- (
              plc._1 match {
                case Leaf(_) => fail(new ShapeError)
                case Node(a, sh) => Monad[ShapeM].pure(a, (deriv, (sh, plc._2)))
              }
            )
          } yield res
      }

    })(ca.length.pred)(ct, ca)

  //============================================================================================
  // TAIL WITH DERIVATIVE
  //

  def tailWithDerivative[A, K <: Nat, N <: Nat, D <: Nat](ct : CardinalTree[A, S[N]], ca : CardinalAddress[K])(
    implicit lte : Lte[K, N, D]
  ) : ShapeM[(TreeSeq[A, S[K], D], CardinalDerivative[TreeSeq[A, S[K], D], K])] =
    poke(cardinalTreeAssoc(lte)(ct), ca)

  //============================================================================================
  // PLUG CARDINAL
  //

  def plugCardinal[A, N <: Nat](n: N)(cd: CardinalDerivative[A, N], a: A) : CardinalTree[A, N] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = CardinalDerivative[A, N] => CardinalTree[A, N]

      def caseZero : Out[_0] = _ => Pt(a)

      def caseSucc[P <: Nat](p: P) : Out[S[P]] = {
        case (cd, deriv) => plugCardinal[Tree[A, S[P]], P](p)(cd, Zipper.plug(S(p))(deriv, a))
      }

    })(n)(cd)


  def getSelection[A, N <: Nat](n: N)(cn: CardinalNesting[A, N], ca: CardinalAddress[N])(sel: A => Boolean) : ShapeM[Tree[Nesting[A, N], N]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (CardinalNesting[A, N], CardinalAddress[N]) => ShapeM[Tree[Nesting[A, N], N]]

      def caseZero : Out[_0] = {
        case (Pt(nst), ca) =>
          if (sel(Nesting.baseValue(nst))) {
            Monad[ShapeM].pure(Pt(nst))
          } else {
            println("Object was not selected ...")
            fail(new ShapeError)
          }
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (cn, tl >> hd) =>
          for {
            pr0 <- poke(cn, tl)
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
            pr0 <- poke(cn, tl)
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
        case (cn, ca, msk) => {
          println("Insering a filler in dim 1")
          Monad[ShapeM].pure(Pt(Node(Dot(a, __1), cn)))
        }
      }

      def caseSucc[P <: Nat](p : P) : (CardinalNestingDblSucc[A, P], CardinalAddress[S[P]], Tree[B, S[P]]) => ShapeM[CardinalNestingDblSucc[A, P]] = {
        case (cn, tl >> hd, msk) =>
          for {
            pr0 <- poke(cn, tl)
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
            pr <- poke[Tree[Nesting[A, S[S[P]]], S[S[P]]], S[P]](cn, ca)
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
            pr <- poke[Tree[Tree[Nesting[A, S[S[S[P]]]], S[S[S[P]]]], S[S[P]]], S[P]](cn, ca)
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

  def extrudeSelection[A[_ <: Nat], N <: Nat, K <: Nat](
    c: Cardinal[A, N], ca: CardinalAddress[K],
    a0 : A[K], a1 : A[S[K]])(p: A[K] => Boolean
  )(implicit diff: Lte.Diff[K, N]) : ShapeM[Cardinal[A, N]] = {

    type KNesting[K <: Nat] = CardinalNesting[A[K], K]

    val lte = diff.lte

    for {
      msk <- getSelection(lte.lower)(Suite.getAt[KNesting, K, N, diff.D](c)(lte), ca)(p)
      res <- Suite.traverse[ShapeM, KNesting, KNesting, S[N]](c)(new Suite.SuiteTraverse[ShapeM, KNesting, KNesting] {
        def apply[M <: Nat](m: M)(cn: KNesting[M]) : ShapeM[KNesting[M]] = {
          println("Passing dimension " ++ natToInt(m).toString)
          extrudeDispatch[A, Nesting[A[K], K], K, M](cn, ca, a0, a1, msk, getFlag(m, lte.lower))
        }
      })
    } yield res
  }

  // dropAtAddress : {A : ℕ → Set} → {n k : ℕ} → Cardinal A n → CardinalAddress k → A (1 + k) → A (2 + k) → (k≤n : k ≤ n) → M (Cardinal A n)
  // dropAtAddress {A} c ca a₀ a₁ k≤n = traverseSuite {{monadIsApp isMonad}} c (λ m cn → dropDispatch {A = A} cn ca a₀ a₁ (getFlag m _))

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



// object Cardinals {

//   //============================================================================================
//   // EXTEND
//   //

//   def extendCardinal[N <: Nat, A](c : Cardinal[N, A])(f : CardinalAddress[S[N]] => A) : Cardinal[S[N], A] = 
//     c match {
//       case (tl >>> hd) => {
//         val extension = mapCardinalTreeWithAddr(c.dim)(hd)({ 
//           case (ca, nst) => Nesting.newNestingExtend(nst)({ 
//             case addr => f(ca >> addr)
//           })
//         })

//         tl >>> hd >>> extension
//       }
//     }

//   //============================================================================================
//   // ROOT ADDRESS
//   //

//   object cardinalRootAddr extends NatCaseSplit0 {

//     type Out[N <: Nat] = CardinalAddress[N]

//     def caseZero : Out[_0] = TNil[Address]() >> Nil

//     def caseSucc[P <: Nat](p : P) : Out[S[P]] =
//       cardinalRootAddr(p) >> Nil

//   }

//   object cardinalRootTree extends NatCaseSplit1 {

//     type Out[N <: Nat, A] = CardinalNesting[N, A] => Option[Tree[N, Nesting[N, A]]]

//     def caseZero[A] : Out[_0, A] = 
//       cn => Some(cn)

//     def caseSucc[P <: Nat, A](p : P) : Out[S[P], A] = 
//       cn => for {
//         res <- poke(cn, cardinalRootAddr(p))
//       } yield res._2

//   }


//   //============================================================================================
//   // SEQ LEAF
//   //

//   def seqLeaf[N <: Nat, K <: Nat, A](n : N, k : K) : TreeSeq[S[N], K, A] = 
//     (new NatCaseSplit {

//       type Out[M <: Nat] = TreeSeq[S[N], M, A]

//       def caseZero : TreeSeq[S[N], _0, A] = 
//         Leaf(S(n))

//       def caseSucc[P <: Nat](p : P) : TreeSeq[S[N], S[P], A] = 
//         Leaf(S(n))

//     })(k)

//   //============================================================================================
//   // EXTRUDE NESTING AT
//   //

//   def extrudeNestingAt[N <: Nat, A, B](a : A, cn : CardinalNesting[S[N], A], ca : CardinalAddress[S[N]], msk : Tree[S[N], B]) : Option[CardinalNesting[S[N], A]] = 
//     ca match {
//       case (ca >> addr) => 
//         for {
//           pr <- poke(cn, ca)
//           (deriv, tr) = pr
//           res <- Nesting.extrudeNesting(a, addr, tr, msk)
//         } yield plugCardinal(msk.dim.pred)(deriv, res)
//     }

//   //============================================================================================
//   // ENCLOSE AT
//   //

//   def encloseAt[N <: Nat, A, B](a : A, addr : Address[S[N]], tr : Tree[S[N], Tree[S[S[N]], A]], msk : Tree[S[N], B]) : Option[Tree[S[N], Tree[S[S[N]], A]]] = 
//     for {
//       zp <- tr seekTo addr
//       (cut, cutSh) <- Tree.excise(zp.focus, msk)
//     } yield close(tr.dim)(zp.context, 
//       Node(Node(a, cut), cutSh)
//     )

//   //============================================================================================
//   // PAD WITH LEAF
//   //

//   def padWithLeaf[N <: Nat, K <: Nat, A, B](k : K, addr : Address[S[N]], seq : TreeSeqDblSucc[S[N], K, A], msk : Tree[S[N], B]) : Option[TreeSeqDblSucc[S[N], K, A]] =
//     for {
//       zp <- seq seekTo addr 
//       (cut, cutSh) <- Tree.excise(zp.focus, msk)
//     } yield close(seq.dim)(zp.context,
//       Node(Node(seqLeaf(S(seq.dim), k), cut), cutSh)
//     )

//   //============================================================================================
//   // DO TAIL
//   //

//   def doTail[K <: Nat, N <: Nat, D <: Nat, A, B](lte : Lte[K, N, D], cn : CardinalNestingDblSucc[N, A], ca : CardinalAddress[K], msk : Tree[K, B]) 
//       : Option[CardinalNestingDblSucc[N, A]] = 
//     (new LteCaseSplit {

//       type Out[K <: Nat, N <: Nat, D <: Nat] = (CardinalNestingDblSucc[N, A], CardinalAddress[K], Tree[K, B]) => Option[CardinalNestingDblSucc[N, A]]

//       def caseZero[N <: Nat](n : N) : Out[_0, N, N] = {
//         case (cn, ca, msk) => 
//           for {
//             pr <- tailWithDerivative[S[N], _0, S[N], Nesting[S[S[N]], A]](cn, ca)(ZeroLte(S(n)))
//           } yield {
//             val (deriv, seq) = pr

//             symm[Nothing, Any, CardinalNestingDblSucc[N, A], CardinalTree[_1, TreeSeq[_2, N, Nesting[S[S[N]], A]]]](
//               cardinalTreeAssoc[S[N], _1, N, Nesting[S[S[N]], A]](SuccLte(ZeroLte(n)))
//             )(
//               plugCardinal(Z)(deriv, Node(seqLeaf(__1, n), Pt(seq)))
//             )
//           }

//       }

//       def caseSucc[K <: Nat, N <: Nat, D <: Nat](plte : Lte[K, N, D]) : Out[S[K], S[N], D] = {
//         case (cn, ca >> addr, msk) => {

//           import Lte.lteSucc

//           type SeqType = TreeSeqDblSucc[S[K], D, Nesting[S[S[S[N]]], A]]
//           type DerivType = CardinalDerivative[K, SeqType]

//           for {
//             pr <- tailWithDerivative[S[S[N]], K, S[S[D]], Nesting[S[S[S[N]]], A]](cn, ca)(lteSucc(lteSucc(plte)))
//             res <- padWithLeaf(plte.diff, addr, pr._2, msk)
//           } yield {

//             type SrcType = CardinalTree[N, Tree[S[N], Tree[S[S[N]], Tree[S[S[S[N]]], Nesting[S[S[S[N]]], A]]]]]
//             type TgtType = CardinalTree[K, Tree[S[K], Tree[S[S[K]], TreeSeq[S[S[S[K]]], D, Nesting[S[S[S[N]]], A]]]]]

//             symm[Nothing, Any, SrcType, TgtType](
//               cardinalTreeAssoc[S[S[N]], S[S[K]], D, Nesting[S[S[S[N]]], A]](SuccLte(SuccLte(plte)))
//             )(
//               plugCardinal(plte.lower)(pr._1, res)
//             )
//           }
//         }
//       }

//     })(lte)(cn, ca, msk)

//   //============================================================================================
//   // DO FILLER
//   //

//   def doFiller[N <: Nat, A, B](a : A, cn : CardinalNesting[S[N], A], ca : CardinalAddress[N], msk : Tree[N, B]) : Option[CardinalNesting[S[N], A]] = 
//     (new NatCaseSplit {

//       type Out[N <: Nat] = (CardinalNestingSucc[N, A], CardinalAddress[N], Tree[N, B]) => Option[CardinalNestingSucc[N, A]]

//       def caseZero : Out[_0] = {
//         case (cn, _ >> addr, msk) => Some(Pt(Node(Dot(a, S(Z)), cn)))
//       }

//       def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
//         case (cn, ca >> hdAddr, msk) => {

//           type SeqType = TreeSeq[S[P], _0, Tree[S[S[P]], Nesting[S[S[P]], A]]]
//           type DerivType = CardinalDerivative[P, SeqType]

//           for {
//             pr <- tailWithDerivative[P, P, _0, Tree[S[S[P]], Nesting[S[S[P]], A]]](cn, ca)(Lte.lteRefl(ca.length.pred))
//             res <- encloseAt[P, Nesting[S[S[P]], A], B](Dot(a, S(ca.length)), hdAddr, pr._2, msk)
//           } yield {
//             plugCardinal[P, SeqType](p)(pr._1, res)
//           }
//         }
//       }

//     })(ca.length.pred)(cn, ca, msk)





//   //============================================================================================
//   // TRAVERSE CARDINAL
//   //

//   // Rewrite this using fold or something ....

//   trait CardinalTraversal[A] {
//     def apply[N <: Nat](n : N, cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]]
//   }

//   def traverseCardinal[N <: Nat, A](trav : CardinalTraversal[A])(cn : Cardinal[N, A]) : Option[Cardinal[N, A]] = 
//     (new NatCaseSplit {

//       type Out[N <: Nat] = Cardinal[N, A] => Option[Cardinal[N, A]]

//       def caseZero : Out[_0] = {
//         case (_ >>> nst) => 
//           for {
//             newNst <- trav(Z, nst)
//           } yield CNil() >>> newNst
//       }

//       def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
//         case (t >>> h) => 
//           for {
//             newTl <- traverseCardinal(trav)(t)
//             newHd <- trav(S(p), h)
//           } yield newTl >>> newHd
//       }

//     })(cn.length.pred)(cn)

//   //============================================================================================
//   // DO EXTRUDE
//   //

//   def doExtrude[N <: Nat, K <: Nat, A, B](
//     a0 : A, a1 : A,
//     msk : Tree[K, B],
//     ca : CardinalAddress[K],
//     c : Cardinal[N, A]
//   ) : Option[Cardinal[N, A]] = {
//     val k : K = msk.dim
//     traverseCardinal(
//       new CardinalTraversal[A] {
//         def apply[N0 <: Nat](n0 : N0, cn : CardinalNesting[N0, A]) : Option[CardinalNesting[N0, A]] = 
//           extrudeDispatch(getFlag(n0, k))(a0, a1, msk, ca, cn)
//       }
//     )(c)
//   }

//   def doDrop[N <: Nat, K <: Nat, A](
//     a0 : A, a1 : A,
//     ca : CardinalAddress[K],
//     c : Cardinal[N, A]
//   ) : Option[Cardinal[N, A]] = 
//     traverseCardinal(new CardinalTraversal[A] {
//       def apply[N <: Nat](n : N, cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]] =
//         dropDispatch(getFlag(n, S(ca.length.pred)))(a0, a1, ca, cn)
//     })(c)

//   def doRootExtrusion[K <: Nat, N <: Nat, D <: Nat, A](k : K)(a0 : A, a1 : A, c : Cardinal[S[N], A])(implicit lte : Lte[K, N, D]) : Option[Cardinal[S[N], A]] = 
//     for {
//       tr <- cardinalRootTree(lte.lower)(c.tail.getAt(lte))
//       res <- doExtrude(a0, a1, tr, cardinalRootAddr(lte.lower), c)
//     } yield res

//   def doTopRootExtrusion[N <: Nat, A](a0 : A, a1 : A, c : Cardinal[N, A]) : Option[Cardinal[S[N], A]] = 
//     doRootExtrusion(c.dim)(a0, a1, extendCardinal(c)(_ => a1))(Lte.lteRefl(c.dim))

//   def getSelectionMask[N <: Nat, A](cn : CardinalNesting[S[N], A], ca : CardinalAddress[S[N]])(p : A => Boolean) : Option[Tree[S[N], Nesting[S[N], A]]] = 
//     ca match {
//       case (ca >> hdAddr) =>
//         for {
//           cd <- poke(cn, ca)
//           zp <- cd._2 seekTo hdAddr
//         } yield Tree.takeWhile(zp._1)(nst => p(nst.baseValue))
//     }

//   def extrudeSelection[K <: Nat, N <: Nat, D <: Nat, A](a0 : A, a1 : A, ca : CardinalAddress[K], c : Cardinal[N, A])(p : A => Boolean)(lte : Lte[K, N, D]) : Option[Cardinal[N, A]] = 
//     (new LteCaseSplit {

//       type Out[K <: Nat, N <: Nat, D <: Nat] = (CardinalAddress[K], Cardinal[N, A]) => Option[Cardinal[N, A]]

//       def caseZero[N <: Nat](n : N) : Out[_0, N, N] =
//         (ca, c) => doExtrude(a0, a1, Pt(()), ca, c)

//       def caseSucc[K <: Nat, N <: Nat, D <: Nat](plte : Lte[K, N, D]) : Out[S[K], S[N], D] = 
//         (ca, c) => {
//           for {
//             msk <- getSelectionMask(c.getAt(SuccLte(plte)), ca)(p)
//             res <- doExtrude(a0, a1, msk, ca, c)
//           } yield res
//         }

//     })(lte)(ca, c)

//   //============================================================================================
//   // OPS CLASS
//   //

//   implicit def cardinalToSigma[M <: Nat, A](c : Cardinal[M, A]) : Sigma[Cardinal, A] =
//     new Sigma[Cardinal, A] {
//       type N = M
//       val n = c.dim
//       val value = c
//     }

//   implicit def cardinalFromSigma[A](cs : Sigma[Cardinal, A]) : Cardinal[cs.N, A] = 
//     cs.value

//   class CardinalOps[N <: Nat, A](c : Cardinal[N, A]) {

//     def dim : N = c.length.pred

//     def toComplexWith[F[_]](gen : CardinalCellGenerator[F, A]) : Complex[N, F[A]] = 
//       Cardinals.toComplex(c)(gen)

//     def toComplex : Complex[N, Polarity[A]] = 
//       Cardinals.toComplex(c)(new CardinalCellGenerator[Polarity, A] {

//         def positive[N <: Nat](n : N) : Polarity[A] = Positive()
//         def negative[N <: Nat](n : N) : Polarity[A] = Negative()

//         def neutral[N <: Nat](a : A) : Polarity[A] = Neutral(a)

//       })
//   }


//   // trait CardinalCellGenerator[F[_], A] {

//   //   def positive[N <: Nat](n : N) : F[A]
//   //   def negative[N <: Nat](n : N) : F[A]

//   //   def neutral[N <: Nat](a : A) : F[A]

//   // }

//   implicit def cardinalToOps[N <: Nat, A](c : Cardinal[N, A]) : CardinalOps[N, A] = 
//     new CardinalOps(c)

//   implicit def cardinalSigmaToOps[A](cs : Sigma[Cardinal, A]) : CardinalOps[cs.N, A] = 
//     new CardinalOps(cs.value)

// }
