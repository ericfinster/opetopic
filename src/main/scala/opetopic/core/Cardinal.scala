/**
  * Cardinal.scala - Opetopic Cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scalaz.Leibniz
import scalaz.Leibniz._

import Nat._
import Zippers._

object Cardinals {

  //============================================================================================
  // TREE SEQUENCES
  //

  trait TreeSeqRec[N <: Nat] extends NatConsRec[AnyRef] {

    type OnZero[+A] = Tree[N, A]

    type OnSucc[P <: Nat, T[+_] <: AnyRef, +A] =
      Tree[N, P#ConsRec[AnyRef, TreeSeqRec[S[N]] , A]]

  }

  type TreeSeq[N <: Nat, K <: Nat, +A] = K#ConsRec[AnyRef, TreeSeqRec[N], A]

  type TreeSeqDblSucc[N <: Nat, P <: Nat, +A] = 
    Tree[N, Tree[S[N], TreeSeq[S[S[N]], P, A]]]

  //============================================================================================
  // AUXILLARY TYPES
  //

  trait CardinalTreeRec extends NatConsRec[AnyRef] {

    type OnZero[+A] = Tree[_0, A]

    type OnSucc[P <: Nat, T[+_] <: AnyRef, +A] = T[Tree[S[P], A]]

  }

  type CardinalTree[N <: Nat, +A] = N#ConsRec[AnyRef, CardinalTreeRec, A]
  type CardinalTreeSucc[P <: Nat, +A] = 
    CardinalTree[P, Tree[S[P], A]]
  type CardinalTreeDblSucc[P <: Nat, +A] = 
    CardinalTree[P, Tree[S[P], Tree[S[S[P]], A]]]

  type CardinalNesting[N <: Nat, +A] = CardinalTree[N, Nesting[N, A]]
  type CardinalNestingSucc[P <: Nat, +A] = 
    CardinalTreeSucc[P, Nesting[S[P], A]]
  type CardinalNestingDblSucc[P <: Nat, +A] = 
    CardinalTreeDblSucc[P, Nesting[S[S[P]], A]]

  type Cardinal[N <: Nat, +A] = ConsSeq[CardinalNesting, S[N], A]

  //============================================================================================
  // CARDINAL ADDRESSES AND DERIVATIVES
  //

  type CardinalAddress[N <: Nat] = TypeSeq[Address, S[N]]

  trait CardinalDerivRec extends NatConsRec[Any] {

    type OnZero[+A] = Unit

    type OnSucc[P <: Nat, T[+_] <: Any, +A] = 
      (T[Tree[S[P], A]], DerivSucc[P, A])

  }

  type CardinalDerivative[N <: Nat, A] = N#ConsRec[Any, CardinalDerivRec, A]

  //============================================================================================
  // TYPE EQUALITY LEMMAS
  //

  def treeSeqAssoc[N <: Nat, M <: Nat, K <: Nat, D <: Nat, A](implicit lte : Lte[K, M, D]) 
      : TreeSeq[N, S[M], A] === TreeSeq[N, K, TreeSeq[S[K#Plus[N]], D, A]] = 
    (new LteCaseSplit {

      type Out[K <: Nat, Y <: Nat, E <: Nat] = 
        TreeSeq[N, S[Y], A] === TreeSeq[N, K, TreeSeq[S[K#Plus[N]], E, A]]

      def caseZero[K <: Nat](k : K) : TreeSeq[N, S[K], A] === TreeSeq[N, _0, TreeSeq[S[N], K, A]] = refl

      def caseSucc[K <: Nat, M <: Nat, D <: Nat](plte : Lte[K, M, D])
          : TreeSeq[N, S[S[M]], A] === TreeSeq[N, S[K], TreeSeq[S[S[K]#Plus[N]], D, A]] = {

        val step1 : Tree[N, Tree[S[N], TreeSeq[S[S[N]], M, A]]] ===
                    Tree[N, TreeSeq[S[N], K, TreeSeq[S[K#Plus[S[N]]], D, A]]] = 
                      lift[Nothing, Nothing, Any, Any, 
                        ({ type L[+B] = Tree[N, B] })#L, 
                        Tree[S[N], TreeSeq[S[S[N]], M, A]], 
                        TreeSeq[S[N], K, TreeSeq[S[K#Plus[S[N]]], D, A]]
                      ](treeSeqAssoc[S[N], M, K, D, A](plte))

        val step2 : Tree[N, TreeSeq[S[N], K, TreeSeq[S[K#Plus[S[N]]], D, A]]] === 
                    Tree[N, TreeSeq[S[N], K, TreeSeq[S[S[K#Plus[N]]], D, A]]] = 
                      lift[Nothing, Nothing, Nat, Any,
                        ({ type L[P <: Nat] = Tree[N, TreeSeq[S[N], K, TreeSeq[S[P], D, A]]] })#L,
                        K#Plus[S[N]], S[K#Plus[N]]
                      ](plusSuccLemma[K, N](plte.lower))

        step1.andThen(step2)

      }

    })(lte)

  def cardinalTreeIsSeq[N <: Nat, A](n : N) : CardinalTree[N, A] === TreeSeq[_0, N, A] = 
    (new NatCaseSplit {

      type Out[M <: Nat] = CardinalTree[M, A] === TreeSeq[_0, M, A]

      def caseZero : CardinalTree[_0, A] === TreeSeq[_0, _0, A] = refl

      def caseSucc[P <: Nat](p : P) : CardinalTree[S[P], A] === TreeSeq[_0, S[P], A] = {

        val step1 : CardinalTree[P, Tree[S[P], A]] === TreeSeq[_0, P, Tree[S[P], A]] = 
          cardinalTreeIsSeq[P, Tree[S[P], A]](p)

        val step2 : TreeSeq[_0, P, Tree[S[P], A]] === TreeSeq[_0, P, TreeSeq[S[P#Plus[_0]], _0, A]] = 
          lift[Nothing, Nothing, Nat, Any,
            ({ type L[Q <: Nat] = TreeSeq[_0, P, Tree[S[Q], A]] })#L,
            P, P#Plus[_0]
          ](plusUnitRight(p))

        val step3 : TreeSeq[_0, P, TreeSeq[S[P#Plus[_0]], _0, A]] === Tree[_0, TreeSeq[_1, P, A]] = 
          symm[Nothing, Any, TreeSeq[_0, S[P], A], TreeSeq[_0, P, TreeSeq[S[P#Plus[_0]], _0, A]]](
            treeSeqAssoc[_0, P, P, _0, A](Lte.lteRefl(p))
          )

        step1.andThen(step2).andThen(step3)

      }

    })(n)


  def cardinalTreeAssoc[N <: Nat, K <: Nat, D <: Nat, A](implicit lte : Lte[K, N, D]) : 
      CardinalTree[S[N], A] === CardinalTree[K, TreeSeq[S[K], D, A]] = {

    val step1 : CardinalTree[S[N], A] === TreeSeq[_0, S[N], A] = 
      cardinalTreeIsSeq(S(lte.upper))

    val step2 : TreeSeq[_0, S[N], A] === TreeSeq[_0, K, TreeSeq[S[K#Plus[_0]], D, A]] =
      treeSeqAssoc

    val step3 : TreeSeq[_0, K, TreeSeq[S[K#Plus[_0]], D, A]] === TreeSeq[_0, K, TreeSeq[S[K], D, A]] = 
      lift[
        Nothing, Nothing, Nat, Any,
        ({ type L[X <: Nat] = TreeSeq[_0, K, TreeSeq[S[X], D, A]] })#L,
        K#Plus[_0], K
      ](symm[Nothing, Nat, K, K#Plus[_0]](plusUnitRight[K](lte.lower)))

    val step4 : TreeSeq[_0, K, TreeSeq[S[K], D, A]] === CardinalTree[K, TreeSeq[S[K], D, A]] = 
      symm[Nothing, Any, 
        CardinalTree[K, TreeSeq[S[K], D, A]], 
        TreeSeq[_0, K, TreeSeq[S[K], D, A]]
      ](cardinalTreeIsSeq(lte.lower))

    step1.andThen(step2).andThen(step3).andThen(step4)

  }

  //============================================================================================
  // MAP CARDINAL TREE
  //

  def mapCardinalTree[N <: Nat, A, B](n : N)(ct : CardinalTree[N, A])(f : A => B) : CardinalTree[N, B] = 
    (new NatCaseSplit {

      type Out[N0 <: Nat] = CardinalTree[N0, A] => CardinalTree[N0, B]

      def caseZero : Out[_0] = {
        case Pt(a) => Pt(f(a))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        cts => mapCardinalTree(p)(cts)(Tree.map(_)(f))
      }

    })(n)(ct)

  //============================================================================================
  // POKE
  //

  def poke[N <: Nat, A](ct : CardinalTree[N, A], ca : CardinalAddress[N]) : Option[(CardinalDerivative[N, A], A)] = 
    (new NatCaseSplit {

      type Out[N0 <: Nat] = (CardinalTree[N0, A], CardinalAddress[N0]) => Option[(CardinalDerivative[N0, A], A)]

      def caseZero : Out[_0] = {
        case (Pt(a), _) => Some((), a)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (ct, tl >> hd) => 
          for {
            res <- poke[P, Tree[S[P], A]](ct, tl)
            (deriv, tr) = res
            plc <- tr seekTo hd 
            res <- (
              plc.focus match {
                case Leaf(_) => None
                case Node(a, sh) => Some((deriv, (sh, plc.context)), a)
              }
            )
          } yield res
      }

    })(ca.length.pred)(ct, ca)

  //============================================================================================
  // TAIL WITH DERIVATIVE
  //

  def tailWithDerivative[N <: Nat, K <: Nat, D <: Nat, A](ct : CardinalTree[S[N], A], ca : CardinalAddress[K])(
    implicit lte : Lte[K, N, D]
  ) : Option[(CardinalDerivative[K, TreeSeq[S[K], D, A]], TreeSeq[S[K], D, A])] = 
    poke(cardinalTreeAssoc(lte)(ct), ca)

  //============================================================================================
  // PLUG CARDINAL
  //

  def plugCardinal[N <: Nat, A](n : N)(cd : CardinalDerivative[N, A], a : A) : CardinalTree[N, A] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = CardinalDerivative[N, A] => CardinalTree[N, A]

      def caseZero : Out[_0] = _ => Pt(a)

      def caseSucc[P <: Nat](p : P) = {
        case (pd, d) => plugCardinal(p)(pd, plug(S(p))(d, a))
      }

    })(n)(cd)

  //============================================================================================
  // EXTEND
  //

  def extend[N <: Nat, A](a : A, c : Cardinal[N, A]) : Cardinal[S[N], A] = 
    c match {
      case (tl >>> hd) => tl >>> hd >>> mapCardinalTree(c.length.pred)(hd)(Nesting.extendNesting(a, _))
    }

  //============================================================================================
  // SEQ LEAF
  //

  def seqLeaf[N <: Nat, K <: Nat, A](n : N, k : K) : TreeSeq[S[N], K, A] = 
    (new NatCaseSplit {

      type Out[M <: Nat] = TreeSeq[S[N], M, A]

      def caseZero : TreeSeq[S[N], _0, A] = 
        Leaf(S(n))

      def caseSucc[P <: Nat](p : P) : TreeSeq[S[N], S[P], A] = 
        Leaf(S(n))

    })(k)

  //============================================================================================
  // EXTRUDE NESTING AT
  //

  def extrudeNestingAt[N <: Nat, A, B](a : A, cn : CardinalNesting[S[N], A], ca : CardinalAddress[S[N]], msk : Tree[S[N], B]) : Option[CardinalNesting[S[N], A]] = 
    ca match {
      case (ca >> addr) => 
        for {
          pr <- poke(cn, ca)
          (deriv, tr) = pr
          res <- Nesting.extrudeNesting(a, addr, tr, msk)
        } yield plugCardinal(msk.dim.pred)(deriv, res)
    }

  //============================================================================================
  // ENCLOSE AT
  //

  def encloseAt[N <: Nat, A](a : A, addr : Address[S[N]], tr : Tree[S[N], Tree[S[S[N]], A]]) : Option[Tree[S[N], Tree[S[S[N]], A]]] = 
    for {
      zp <- tr seekTo addr
      flt <- Tree.flatten(zp.focus)
    } yield close(tr.dim)(zp.context, 
      Node(Node(a, zp.focus), flt.constWith(Leaf(tr.dim)))
    )

  //============================================================================================
  // PAD WITH LEAF
  //

  def padWithLeaf[N <: Nat, K <: Nat, A](k : K, addr : Address[S[N]], seq : TreeSeqDblSucc[S[N], K, A]) : Option[TreeSeqDblSucc[S[N], K, A]] =
    for {
      zp <- seq seekTo addr 
      flt <- Tree.flatten(zp.focus)
    } yield close(seq.dim)(zp.context,
      Node(Node(seqLeaf(S(seq.dim), k), zp.focus), flt.constWith(Leaf(seq.dim)))
    )

  //============================================================================================
  // DO TAIL
  //

  def doTail[K <: Nat, N <: Nat, D <: Nat, A](lte : Lte[K, N, D], cn : CardinalNestingDblSucc[N, A], ca : CardinalAddress[K]) 
      : Option[CardinalNestingDblSucc[N, A]] = 
    (new LteCaseSplit {

      type Out[K <: Nat, N <: Nat, D <: Nat] = (CardinalNestingDblSucc[N, A], CardinalAddress[K]) => Option[CardinalNestingDblSucc[N, A]]

      def caseZero[N <: Nat](n : N) : Out[_0, N, N] = {
        case (cn, ca) => 
          for {
            pr <- tailWithDerivative[S[N], _0, S[N], Nesting[S[S[N]], A]](cn, ca)(ZeroLte(S(n)))
          } yield {
            val (deriv, seq) = pr

            symm[Nothing, Any, CardinalNestingDblSucc[N, A], CardinalTree[_1, TreeSeq[_2, N, Nesting[S[S[N]], A]]]](
              cardinalTreeAssoc[S[N], _1, N, Nesting[S[S[N]], A]](SuccLte(ZeroLte(n)))
            )(
              plugCardinal(Z)(deriv, Node(seqLeaf(__1, n), Pt(seq)))
            )
          }

      }

      def caseSucc[K <: Nat, N <: Nat, D <: Nat](plte : Lte[K, N, D]) : Out[S[K], S[N], D] = {
        case (cn, ca >> addr) => {

          import Lte.lteSucc

          type SeqType = TreeSeqDblSucc[S[K], D, Nesting[S[S[S[N]]], A]]
          type DerivType = CardinalDerivative[K, SeqType]

          for {
            pr <- tailWithDerivative[S[S[N]], K, S[S[D]], Nesting[S[S[S[N]]], A]](cn, ca)(lteSucc(lteSucc(plte)))
            res <- padWithLeaf(plte.diff, addr, pr._2)
          } yield {

            type SrcType = CardinalTree[N, Tree[S[N], Tree[S[S[N]], Tree[S[S[S[N]]], Nesting[S[S[S[N]]], A]]]]]
            type TgtType = CardinalTree[K, Tree[S[K], Tree[S[S[K]], TreeSeq[S[S[S[K]]], D, Nesting[S[S[S[N]]], A]]]]]

            symm[Nothing, Any, SrcType, TgtType](
              cardinalTreeAssoc[S[S[N]], S[S[K]], D, Nesting[S[S[S[N]]], A]](SuccLte(SuccLte(plte)))
            )(
              plugCardinal(plte.lower)(pr._1, pr._2)
            )
          }
        }
      }

    })(lte)(cn, ca)

  //============================================================================================
  // DO FILLER
  //

  def doFiller[N <: Nat, A](a : A, cn : CardinalNesting[S[N], A], ca : CardinalAddress[N]) : Option[CardinalNesting[S[N], A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = (CardinalNestingSucc[N, A], CardinalAddress[N]) => Option[CardinalNestingSucc[N, A]]

      def caseZero : Out[_0] = {
        case (cn, _ >> addr) => Some(Pt(Node(Dot(a, S(Z)), cn)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (cn, ca >> hdAddr) => {

          type SeqType = TreeSeq[S[P], _0, Tree[S[S[P]], Nesting[S[S[P]], A]]]
          type DerivType = CardinalDerivative[P, SeqType]

          for {
            pr <- tailWithDerivative[P, P, _0, Tree[S[S[P]], Nesting[S[S[P]], A]]](cn, ca)(Lte.lteRefl(ca.length.pred))
            res <- encloseAt[P, Nesting[S[S[P]], A]](Dot(a, S(ca.length)), hdAddr, pr._2)
          } yield {
            plugCardinal[P, SeqType](p)(pr._1, res)
          }
        }
      }

    })(ca.length.pred)(cn, ca)

  //============================================================================================
  // DIMENSION FLAGS
  //

  sealed trait CardinalDimFlag[N <: Nat, K <: Nat] { 
    def succ : CardinalDimFlag[S[N], S[K]] 
    def extrudeDispatch[A, B](a0 : A, a1 : A, msk : Tree[K, B], ca : CardinalAddress[K], cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]]
  }

  case class DimEq[K <: Nat](k : K) extends CardinalDimFlag[K, K] {
    def succ : CardinalDimFlag[S[K], S[K]] = DimEq(S(k))

    def extrudeDispatch[A, B](a0 : A, a1 : A, msk : Tree[K, B], ca : CardinalAddress[K], cn : CardinalNesting[K, A]) : Option[CardinalNesting[K, A]] =
      (new NatCaseSplit {

        type Out[N <: Nat] = (Tree[N, B], CardinalAddress[N], CardinalNesting[N, A]) => Option[CardinalNesting[N, A]]

        def caseZero : Out[_0] = {
          (msk, ca, cn) => Some(Pt(Box(a0, cn)))
        }

        def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
          (msk, ca, cn) => extrudeNestingAt(a0, cn, ca, msk)
        }

      })(msk.dim)(msk, ca, cn)

  }


  case class DimSucc[K <: Nat](k : K) extends CardinalDimFlag[S[K], K] {
    def succ : CardinalDimFlag[S[S[K]], S[K]] = DimSucc(S(k))
    def extrudeDispatch[A, B](a0 : A, a1 : A, msk : Tree[K, B], ca : CardinalAddress[K], cn : CardinalNesting[S[K], A]) : Option[CardinalNesting[S[K], A]] = 
      doFiller(a1, cn, ca)
  }

  case class DimLt[N <: Nat, K <: Nat, D <: Nat](slte : Lte[S[N], K, D]) extends CardinalDimFlag[N, K] {
    def succ : CardinalDimFlag[S[N], S[K]] = DimLt(SuccLte(slte))
    def extrudeDispatch[A, B](a0 : A, a1 : A, msk : Tree[K, B], ca : CardinalAddress[K], cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]] = 
      Some(cn)
  }

  case class DimDblSucc[N <: Nat, K <: Nat, D <: Nat](sslte : Lte[S[S[K]], N, D]) extends CardinalDimFlag[N, K] {
    def succ : CardinalDimFlag[S[N], S[K]] = DimDblSucc(SuccLte(sslte))
    def extrudeDispatch[A, B](a0 : A, a1 : A, msk : Tree[K, B], ca : CardinalAddress[K], cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]] = 
      (new NatCaseSplitWithOne {  

        type Out[N <: Nat] = (Lte[S[S[K]], N, D], CardinalNesting[N, A]) => Option[CardinalNesting[N, A]]

        def caseZero : Out[_0] = {
          case (_, _) => None  // Unreachable
        }

        def caseOne : Out[_1] = {
          case (_, _) => None // Unreachable
        }

        def caseDblSucc[P <: Nat](p : P) : (Lte[S[S[K]], S[S[P]], D], CardinalNestingDblSucc[P, A]) => Option[CardinalNestingDblSucc[P, A]] = {
          case (SuccLte(SuccLte(lte)), cn) => doTail(lte, cn, ca)
        }

      })(sslte.upper)(sslte, cn)

  }

  def getFlag[N <: Nat, K <: Nat](n : N, k : K) : CardinalDimFlag[N, K] = 
    (new NatCaseSplit {

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
        (new NatCaseSplit {

          type Out[N <: Nat] = CardinalDimFlag[N, S[P]]

          def caseZero : Out[_0] = DimLt(SuccLte(ZeroLte(p)))
          def caseSucc[PN <: Nat](pn : PN) : Out[S[PN]] = getFlag[PN, P](pn, p).succ
 
        })(n)

    })(k)

  //============================================================================================
  // TRAVERSE CARDINAL
  //

  trait CardinalTraversal[A] {
    def apply[N <: Nat](n : N, cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]]
  }

  def traverseCardinal[N <: Nat, A](trav : CardinalTraversal[A])(cn : Cardinal[N, A]) : Option[Cardinal[N, A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = Cardinal[N, A] => Option[Cardinal[N, A]]

      def caseZero : Out[_0] = {
        case (_ >>> nst) => 
          for {
            newNst <- trav(Z, nst)
          } yield CNil() >>> newNst
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (t >>> h) => 
          for {
            newTl <- traverseCardinal(trav)(t)
            newHd <- trav(S(p), h)
          } yield newTl >>> newHd
      }

    })(cn.length.pred)(cn)

  //============================================================================================
  // DO EXTRUDE
  //

  def doExtrude[N <: Nat, K <: Nat, A, B](
    a0 : A, a1 : A,
    msk : Tree[K, B],
    ca : CardinalAddress[K],
    c : Cardinal[N, A]
  ) : Option[Cardinal[N, A]] = {
    val k : K = msk.dim
    traverseCardinal(
      new CardinalTraversal[A] {
        def apply[N0 <: Nat](n0 : N0, cn : CardinalNesting[N0, A]) : Option[CardinalNesting[N0, A]] = 
          getFlag(n0, k).extrudeDispatch(a0, a1, msk, ca, cn)
      }
    )(c)
  }

}
