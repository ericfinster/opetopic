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

  //============================================================================================
  // AUXILLARY TYPES
  //

  trait CardinalTreeRec extends NatConsRec[AnyRef] {

    type OnZero[+A] = Tree[_0, A]

    type OnSucc[P <: Nat, T[+_] <: AnyRef, +A] =
      P#ConsRec[AnyRef, CardinalTreeRec, Tree[S[P], A]]

  }

  type CardinalTree[N <: Nat, +A] = N#ConsRec[AnyRef, CardinalTreeRec, A]
  type CardinalNesting[N <: Nat, +A] = CardinalTree[N, Nesting[N, A]]

  type Cardinal[N <: Nat, +A] = DList[CardinalNesting, S[N], A]

  //============================================================================================
  // CARDINAL ADDRESSES AND DERIVATIVES
  //

  // type CardinalAddress[N <: Nat] = DimSeq0[Address, N]

  // sealed trait CardinalDerivative[N <: Nat, +A] { def dim : N }
  // case object InitDeriv extends CardinalDerivative[_0, Nothing] { def dim = Z }
  // case class NextCard[N <: Nat, +A](cd : CardinalDerivative[N, Tree[S[N], A]], d : Derivative[S[N], A]) extends CardinalDerivative[S[N], A] {
  //   def dim = S(cd.dim)
  // }

  //============================================================================================
  // TYPE EQUALITY LEMMAS
  //

  // def treeSeqAssoc[N <: Nat, M <: Nat, K <: Nat, D <: Nat, A](implicit lte : Lte[K, M, D]) 
  //     : TreeSeq[N, S[M], A] === TreeSeq[N, K, TreeSeq[S[K#Plus[N]], D, A]] = 
  //   (new LteSimpleMatch {

  //     type Out[X <: Nat, Y <: Nat, E <: Nat] = 
  //       TreeSeq[N, S[Y], A] === TreeSeq[N, X, TreeSeq[S[X#Plus[N]], E, A]]

  //     def caseZero[X <: Nat](x : X) : TreeSeq[N, S[X], A] === TreeSeq[N, _0, TreeSeq[S[N], X, A]] = refl

  //     def caseSucc[X <: Nat, Y <: Nat, E <: Nat](plte : Lte[X, Y, E])
  //         : TreeSeq[N, S[S[Y]], A] === TreeSeq[N, S[X], TreeSeq[S[S[X]#Plus[N]], E, A]] = {

  //       // In above, we have X = K, Y = M, N = N

  //       val step1 : Tree[N, Tree[S[N], TreeSeq[S[S[N]], Y, A]]] ===
  //                   Tree[N, TreeSeq[S[N], X, TreeSeq[S[X#Plus[S[N]]], E, A]]] = 
  //                     lift[Nothing, Nothing, Any, Any, 
  //                       ({ type L[+B] = Tree[N, B] })#L, 
  //                       Tree[S[N], TreeSeq[S[S[N]], Y, A]], 
  //                       TreeSeq[S[N], X, TreeSeq[S[X#Plus[S[N]]], E, A]]
  //                     ](treeSeqAssoc[S[N], Y, X, E, A](plte))

  //       val step2 : Tree[N, TreeSeq[S[N], X, TreeSeq[S[X#Plus[S[N]]], E, A]]] === 
  //                   Tree[N, TreeSeq[S[N], X, TreeSeq[S[S[X#Plus[N]]], E, A]]] = 
  //                     lift[Nothing, Nothing, Nat, Any,
  //                       ({ type L[P <: Nat] = Tree[N, TreeSeq[S[N], X, TreeSeq[S[P], E, A]]] })#L,
  //                       X#Plus[S[N]], S[X#Plus[N]]
  //                     ](plusSuccLemma[X, N](plte.lower))

  //       step1.andThen(step2)

  //     }

  //   })(lte)

  // def cardinalTreeIsSeq[N <: Nat, A](n : N) : CardinalTree[N, A] === TreeSeq[_0, N, A] = 
  //   (new NatElim {

  //     type Out[M <: Nat] = CardinalTree[M, A] === TreeSeq[_0, M, A]

  //     def caseZero : CardinalTree[_0, A] === TreeSeq[_0, _0, A] = refl

  //     def caseSucc[P <: Nat](p : P, ih : Out[P]) : CardinalTree[S[P], A] === TreeSeq[_0, S[P], A] = {

  //       val step1 : CardinalTree[P, Tree[S[P], A]] === TreeSeq[_0, P, Tree[S[P], A]] = 
  //         cardinalTreeIsSeq[P, Tree[S[P], A]](p)

  //       val step2 : TreeSeq[_0, P, Tree[S[P], A]] === TreeSeq[_0, P, TreeSeq[S[P#Plus[_0]], _0, A]] = 
  //         lift[Nothing, Nothing, Nat, Any,
  //           ({ type L[Q <: Nat] = TreeSeq[_0, P, Tree[S[Q], A]] })#L,
  //           P, P#Plus[_0]
  //         ](plusUnitRight(p))

  //       val step3 : TreeSeq[_0, P, TreeSeq[S[P#Plus[_0]], _0, A]] === Tree[_0, TreeSeq[_1, P, A]] = 
  //         symm[Nothing, Any, TreeSeq[_0, S[P], A], TreeSeq[_0, P, TreeSeq[S[P#Plus[_0]], _0, A]]](
  //           treeSeqAssoc[_0, P, P, _0, A](Lte.lteRefl(p))
  //         )

  //       step1.andThen(step2).andThen(step3)

  //     }

  //   })(n)


  // def cardinalTreeAssoc[N <: Nat, K <: Nat, D <: Nat, A](implicit lte : Lte[K, N, D]) : 
  //     CardinalTree[S[N], A] === CardinalTree[K, TreeSeq[S[K], D, A]] = {

  //   val step1 : CardinalTree[S[N], A] === TreeSeq[_0, S[N], A] = 
  //     cardinalTreeIsSeq(S(lte.upper))

  //   val step2 : TreeSeq[_0, S[N], A] === TreeSeq[_0, K, TreeSeq[S[K#Plus[_0]], D, A]] =
  //     treeSeqAssoc

  //   val step3 : TreeSeq[_0, K, TreeSeq[S[K#Plus[_0]], D, A]] === TreeSeq[_0, K, TreeSeq[S[K], D, A]] = 
  //     lift[
  //       Nothing, Nothing, Nat, Any,
  //       ({ type L[X <: Nat] = TreeSeq[_0, K, TreeSeq[S[X], D, A]] })#L,
  //       K#Plus[_0], K
  //     ](symm[Nothing, Nat, K, K#Plus[_0]](plusUnitRight[K](lte.lower)))

  //   val step4 : TreeSeq[_0, K, TreeSeq[S[K], D, A]] === CardinalTree[K, TreeSeq[S[K], D, A]] = 
  //     symm[Nothing, Any, 
  //       CardinalTree[K, TreeSeq[S[K], D, A]], 
  //       TreeSeq[_0, K, TreeSeq[S[K], D, A]]
  //     ](cardinalTreeIsSeq(lte.lower))

  //   step1.andThen(step2).andThen(step3).andThen(step4)

  // }

  //============================================================================================
  // MAP CARDINAL TREE
  //

  // def mapCardinalTree[N <: Nat, A, B](n : N)(ct : CardinalTree[N, A])(f : A => B) : CardinalTree[N, B] = 
  //   (new RealNatCaseSplit {

  //     type Out[N0 <: Nat] = CardinalTree[N0, A] => CardinalTree[N0, B]

  //     def caseZero : Out[_0] = {
  //       case Pt(a) => Pt(f(a))
  //     }

  //     def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
  //       cts => mapCardinalTree(p)(cts)(Tree.map(_)(f))
  //     }

  //   })(n)(ct)

  //============================================================================================
  // POKE
  //

  // def poke[N <: Nat, A](ct : CardinalTree[N, A], ca : CardinalAddress[N]) : Option[(CardinalDerivative[N, A], A)] = 
  //   (new RealNatCaseSplit {

  //     type Out[N0 <: Nat] = (CardinalTree[N0, A], CardinalAddress[N0]) => Option[(CardinalDerivative[N0, A], A)]

  //     def caseZero : Out[_0] = {
  //       case (Pt(a), _) => Some((InitDeriv, a))
  //     }

  //     def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
  //       case (ct, tl >:> hd) => 
  //         for {
  //           res <- poke[P, Tree[S[P], A]](ct, tl)
  //           (deriv, tr) = res
  //           plc <- Tree.seekTo(tr, hd)
  //           res <- (
  //             plc.focus match {
  //               case Leaf(_) => None
  //               case Node(a, sh) => Some((NextCard(deriv, Open(sh, plc.context)), a))
  //             }
  //           )
  //         } yield res
  //     }

  //   })(dim(ca))(ct, ca)

  //============================================================================================
  // TAIL WITH DERIVATIVE
  //

  // def tailWithDerivative[N <: Nat, K <: Nat, D <: Nat, A](ct : CardinalTree[S[N], A], ca : CardinalAddress[K])(
  //   implicit lte : Lte[K, N, D]
  // ) : Option[(CardinalDerivative[K, TreeSeq[S[K], D, A]], TreeSeq[S[K], D, A])] = 
  //   poke(cardinalTreeAssoc(lte)(ct), ca)

  //============================================================================================
  // PLUG CARDINAL
  //

  // def plugCardinal[N <: Nat, A](cd : CardinalDerivative[N, A], a : A) : CardinalTree[N, A] = 
  //   (new NatCaseSplit {

  //     type In[M <: Nat] = CardinalDerivative[M, A]
  //     type Out[M <: Nat] = CardinalTree[M, A]

  //     def caseZero(cd : CardinalDerivative[_0, A]) : CardinalTree[_0, A] = Pt(a)

  //     def caseSucc[P <: Nat](cd : CardinalDerivative[S[P], A]) =
  //       cd match {
  //         case NextCard(pd, d) => plugCardinal(pd, Derivative.plug(d, a))
  //       }

  //   })(cd.dim, cd)

  //============================================================================================
  // EXTEND
  //

  // def extend[N <: Nat, A](a : A, c : Cardinal[N, A]) : Cardinal[S[N], A] = 
  //   c match {
  //     case (tl :>> hd) => tl :>> hd :>> mapCardinalTree(dim(c))(hd)(Nesting.extendNesting(a, _))
  //   }

  //============================================================================================
  // SEQ LEAF
  //

  // def seqLeaf[N <: Nat, K <: Nat, A](n : N, k : K) : TreeSeq[S[N], K, A] = 
  //   (new NatCaseSplit {

  //     type In[M <: Nat] = Unit
  //     type Out[M <: Nat] = TreeSeq[S[N], M, A]

  //     def caseZero(in : Unit) : TreeSeq[S[N], _0, A] = Leaf(S(n))
  //     def caseSucc[P <: Nat](in : Unit) : TreeSeq[S[N], S[P], A] = Leaf(S(n))

  //   })(k, ())

  //============================================================================================
  // EXTRUDE NESTING AT
  //

  // def extrudeNestingAt[N <: Nat, A, B](a : A, cn : CardinalNesting[S[N], A], ca : CardinalAddress[S[N]], msk : Tree[S[N], B]) : Option[CardinalNesting[S[N], A]] = 
  //   ca match {
  //     case (ca >:> addr) => 
  //       for {
  //         pr <- poke(cn, ca)
  //         (deriv, tr) = pr
  //         res <- Nesting.extrudeNesting(a, addr, tr, msk)
  //       } yield plugCardinal(deriv, res)
  //   }

  //============================================================================================
  // ENCLOSE AT
  //

  // def encloseAt[N <: Nat, A](a : A, addr : Address[S[N]], tr : Tree[S[N], Tree[S[S[N]], A]]) : Option[Tree[S[N], Tree[S[S[N]], A]]] = 
  //   for {
  //     zp <- Tree.seekTo(tr, addr)
  //     flt <- Tree.flatten(zp.focus)
  //   } yield Context.close(zp.context, 
  //     Node(Node(a, zp.focus), Tree.const(flt)(Leaf(tr.dim)))
  //   )

  //============================================================================================
  // PAD WITH LEAF
  //

  // type PrefixSeq[N <: Nat, K <: Nat, +A] = 
  //   Tree[N, Tree[S[N], TreeSeq[S[S[N]], K, A]]]

  // def padWithLeaf[N <: Nat, K <: Nat, A](k : K, addr : Address[S[N]], seq : PrefixSeq[S[N], K, A]) : Option[PrefixSeq[S[N], K, A]] =
  //   for {
  //     zp <- Tree.seekTo(seq, addr)
  //     flt <- Tree.flatten(zp.focus)
  //   } yield Context.close(zp.context,
  //     Node(Node(seqLeaf(S(addr.dim), k), zp.focus), Tree.const(flt)(Leaf(seq.dim)))
  //   )

  //============================================================================================
  // DO TAIL
  //

  // type PrefixNesting[N <: Nat, A] = 
  //   CardinalTree[N, Tree[S[N], Tree[S[S[N]], Nesting[S[S[N]], A]]]]

  // def doTail[K <: Nat, N <: Nat, D <: Nat, A](lte : Lte[K, N, D], cn : PrefixNesting[N, A], ca : CardinalAddress[K]) : Option[PrefixNesting[N, A]] = 
  //   (new LteTwoParamCaseSplit {

  //     type In0[K0 <: Nat, N0 <: Nat, D0 <: Nat] = PrefixNesting[N0, A]
  //     type In1[K0 <: Nat, N0 <: Nat, D0 <: Nat] = CardinalAddress[K0]
  //     type Out[K0 <: Nat, N0 <: Nat, D0 <: Nat] = Option[PrefixNesting[N0, A]]

  //     def caseZeroLte[N0 <: Nat](n0 : N0, pn0 : PrefixNesting[N0, A], ca0 : CardinalAddress[_0]) : Option[PrefixNesting[N0, A]] = {
  //       for {
  //         pr <- tailWithDerivative[S[N0], _0, S[N0], Nesting[S[S[N0]], A]](pn0, ca0)(ZeroLte(S(n0)))
  //       } yield {
  //         val (deriv, seq) = pr

  //         symm[Nothing, Any, PrefixNesting[N0, A], CardinalTree[_1, TreeSeq[_2, N0, Nesting[S[S[N0]], A]]]](
  //           cardinalTreeAssoc[S[N0], _1, N0, Nesting[S[S[N0]], A]](SuccLte(ZeroLte(n0)))
  //         )(
  //           plugCardinal(deriv, Node(seqLeaf(__1, n0), Pt(seq)))
  //         )

  //       }
  //     }

  //     def caseSuccLte[K0 <: Nat, N0 <: Nat, D0 <: Nat](plte : Lte[K0, N0, D0], in0 : In0[S[K0], S[N0], D0], in1 : In1[S[K0], S[N0], D0]) : Out[S[K0], S[N0], D0] = {

  //       import Lte.lteSucc

  //       type SeqType = Tree[S[K0], Tree[S[S[K0]], TreeSeq[S[S[S[K0]]], D0, Nesting[S[S[S[N0]]], A]]]]
  //       type DerivType = CardinalDerivative[K0, SeqType]

  //       in1 match {
  //         case (ca >:> addr) => 
  //           for {
  //             pr <- tailWithDerivative[S[S[N0]], K0, S[S[D0]], Nesting[S[S[S[N0]]], A]](in0, ca)(lteSucc(lteSucc(plte)))
  //             (deriv : DerivType) = pr._1
  //             (seq : SeqType) = pr._2
  //             res <- padWithLeaf(plte.diff, addr, seq)
  //           } yield {

  //             type SrcType = CardinalTree[N0, Tree[S[N0], Tree[S[S[N0]], Tree[S[S[S[N0]]], Nesting[S[S[S[N0]]], A]]]]]
  //             type TgtType = CardinalTree[K0, Tree[S[K0], Tree[S[S[K0]], TreeSeq[S[S[S[K0]]], D0, Nesting[S[S[S[N0]]], A]]]]]

  //             symm[Nothing, Any, SrcType, TgtType](
  //               cardinalTreeAssoc[S[S[N0]], S[S[K0]], D0, Nesting[S[S[S[N0]]], A]](SuccLte(SuccLte(plte)))
  //             )(
  //               plugCardinal(deriv, seq)
  //             )

  //           }
  //       }
  //     }


  //   })(lte, cn, ca)

  //============================================================================================
  // DO FILLER
  //

  // def doFiller[N <: Nat, A](a : A, cn : CardinalNesting[S[N], A], ca : CardinalAddress[N]) : Option[CardinalNesting[S[N], A]] = 
  //   (new NatCaseSplitTwo {

  //     type In0[N0 <: Nat] = CardinalNesting[S[N0], A]
  //     type In1[N0 <: Nat] = CardinalAddress[N0]
  //     type Out[N0 <: Nat] = Option[CardinalNesting[S[N0], A]]

  //     def caseZero(cn0 : CardinalNesting[_1, A], ca0 : CardinalAddress[_0]) : Option[CardinalNesting[_1, A]] = 
  //       ca0 match {
  //         case (_ >:> addr) => Some(Pt(Node(Dot(a, S(Z)), cn0)))
  //       }

  //     type NestingType[P <: Nat] = Nesting[S[S[P]], A]
  //     type CardinalType[P <: Nat] = CardinalTree[P, Tree[S[P], Tree[S[S[P]], NestingType[P]]]]

  //     def caseSucc[P <: Nat](cns : CardinalType[P], cas : CardinalAddress[S[P]]) : Option[CardinalNesting[S[S[P]], A]] = {

  //       type SeqType = TreeSeq[S[P], _0, Tree[S[S[P]], NestingType[P]]]
  //       type DerivType = CardinalDerivative[P, SeqType]

  //       cas match {
  //         case (cap >:> hdAddr) => 
  //           for {
  //             pr <- tailWithDerivative[P, P, _0, Tree[S[S[P]], NestingType[P]]](cns, cap)(Lte.lteRefl(dim(cap)))
  //             (deriv : DerivType, seq : SeqType) = pr
  //             res <- encloseAt[P, Nesting[S[S[P]], A]](Dot(a, S(dim(cas))), hdAddr, seq)
  //           } yield {
  //             plugCardinal[P, SeqType](deriv, res)
  //           }
  //       }
  //     }

  //   })(dim(ca), cn, ca)

  //============================================================================================
  // DIMENSION FLAGS
  //

  // sealed trait CardinalDimFlag[N <: Nat, K <: Nat] { 
  //   def succ : CardinalDimFlag[S[N], S[K]] 
  //   def extrudeDispatch[A, B](a0 : A, a1 : A, msk : Tree[K, B], ca : CardinalAddress[K], cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]]
  // }

  // case class DimEq[K <: Nat](k : K) extends CardinalDimFlag[K, K] {
  //   def succ : CardinalDimFlag[S[K], S[K]] = DimEq(S(k))

  //   def extrudeDispatch[A, B](a0 : A, a1 : A, msk : Tree[K, B], ca : CardinalAddress[K], cn : CardinalNesting[K, A]) : Option[CardinalNesting[K, A]] =
  //     (new NatCaseSplit {

  //       type In[N <: Nat] = Tree[N, B]
  //       type Out[N <: Nat] = Function2[CardinalAddress[N], CardinalNesting[N, A], Option[CardinalNesting[N, A]]]

  //       def caseZero(msk0 : Tree[_0, B]) : Out[_0] = {
  //         (ca0, cn0) => Some(Pt(Box(a0, cn0)))
  //       }

  //       def caseSucc[P <: Nat](msks : Tree[S[P], B]) : Out[S[P]] = {
  //         (cas, cns) => extrudeNestingAt(a0, cns, cas, msks)
  //       }

  //     })(msk.dim, msk)(ca, cn)

  // }


  // case class DimSucc[K <: Nat](k : K) extends CardinalDimFlag[S[K], K] {
  //   def succ : CardinalDimFlag[S[S[K]], S[K]] = DimSucc(S(k))
  //   def extrudeDispatch[A, B](a0 : A, a1 : A, msk : Tree[K, B], ca : CardinalAddress[K], cn : CardinalNesting[S[K], A]) : Option[CardinalNesting[S[K], A]] = 
  //     doFiller(a1, cn, ca)
  // }

  // case class DimLt[N <: Nat, K <: Nat, D <: Nat](slte : Lte[S[N], K, D]) extends CardinalDimFlag[N, K] {
  //   def succ : CardinalDimFlag[S[N], S[K]] = DimLt(SuccLte(slte))
  //   def extrudeDispatch[A, B](a0 : A, a1 : A, msk : Tree[K, B], ca : CardinalAddress[K], cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]] = 
  //     Some(cn)
  // }

  // case class DimDblSucc[N <: Nat, K <: Nat, D <: Nat](sslte : Lte[S[S[K]], N, D]) extends CardinalDimFlag[N, K] {
  //   def succ : CardinalDimFlag[S[N], S[K]] = DimDblSucc(SuccLte(sslte))
  //   def extrudeDispatch[A, B](a0 : A, a1 : A, msk : Tree[K, B], ca : CardinalAddress[K], cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]] = 
  //     (new NatCaseSplit1Two {

  //       type In0[N0 <: Nat] = Lte[S[S[K]], N0, D]
  //       type In1[N0 <: Nat] = CardinalNesting[N0, A]
  //       type Out[N0 <: Nat] = Option[CardinalNesting[N0, A]]

  //       def caseZero(in0 : In0[_0], in1 : In1[_0]) : Out[_0] = None  // Unreachable
  //       def caseOne(in0 : In0[_1], in1 : In1[_1]) : Out[_1] = None  // Unreachable

  //       def caseDblSucc[P <: Nat](l : Lte[S[S[K]], S[S[P]], D], c : PrefixNesting[P, A]) : Out[S[S[P]]] = 
  //         l match {
  //           case SuccLte(SuccLte(lte)) => doTail(lte, c, ca)
  //         }

  //     })(sslte.upper, sslte, cn)

  // }

  // def getFlag[N <: Nat, K <: Nat](n : N, k : K) : CardinalDimFlag[N, K] = 
  //   (new NatCaseSplit {

  //     type In[K0 <: Nat] = K0
  //     type Out[K0 <: Nat] = CardinalDimFlag[N, K0]

  //     def caseZero(zk : _0) : CardinalDimFlag[N, _0] = 
  //       ( new NatCaseSplit1 {

  //         type In[N0 <: Nat] = N0
  //         type Out[N0 <: Nat] = CardinalDimFlag[N0, _0]

  //         def caseZero(zn : _0) : CardinalDimFlag[_0, _0] = DimEq(Z)
  //         def caseOne(on : _1) : CardinalDimFlag[_1, _0] = DimSucc(Z)
  //         def caseDblSucc[P <: Nat](ssp : S[S[P]]) : CardinalDimFlag[S[S[P]], _0] = 
  //           DimDblSucc(SuccLte(SuccLte(ZeroLte(ssp.pred.pred))))

  //       })(n, n)

  //     def caseSucc[PK <: Nat](spk : S[PK]) : CardinalDimFlag[N, S[PK]] = 
  //       (new NatCaseSplit {

  //         type In[N0 <: Nat] = N0
  //         type Out[N0 <: Nat] = CardinalDimFlag[N0, S[PK]]

  //         def caseZero(zn : _0) : CardinalDimFlag[_0, S[PK]] = DimLt(SuccLte(ZeroLte(spk.pred)))
  //         def caseSucc[PN <: Nat](spn : S[PN]) : CardinalDimFlag[S[PN], S[PK]] = 
  //           getFlag[PN, PK](spn.pred, spk.pred).succ

  //       })(n, n)

  //   })(k, k)

  //============================================================================================
  // TRAVERSE CARDINAL
  //

  // trait CardinalTraversal[A] {
  //   def execute[N <: Nat](n : N, cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]]
  // }

  // def traverseCardinal[N <: Nat, A](trav : CardinalTraversal[A])(cn : Cardinal[N, A]) : Option[Cardinal[N, A]] = 
  //   (new NatCaseSplit {

  //     type In[N0 <: Nat] = Cardinal[N0, A]
  //     type Out[N0 <: Nat] = Option[Cardinal[N0, A]]

  //     def caseZero(c0 : Cardinal[_0, A]) : Option[Cardinal[_0, A]] = 
  //       c0 match {
  //         case (_ :>> hd) => 
  //           for {
  //             newHd <- trav.execute(Z, hd)
  //           } yield NilCard :>> newHd
  //       }

  //     def caseSucc[P <: Nat](cp : Cardinal[S[P], A]) : Option[Cardinal[S[P], A]] = 
  //       cp match {
  //         case (t :>> h) => 
  //           for {
  //             newTl <- traverseCardinal(trav)(t)
  //             newHd <- trav.execute(dim(cp), h)
  //           } yield newTl :>> newHd
  //       }

  //   })(dim(cn), cn)

  //============================================================================================
  // DO EXTRUDE
  //

  // def doExtrude[N <: Nat, K <: Nat, A, B](
  //   a0 : A, a1 : A,
  //   msk : Tree[K, B],
  //   ca : CardinalAddress[K],
  //   c : Cardinal[N, A]
  // ) : Option[Cardinal[N, A]] = {
  //   val k : K = msk.dim
  //   traverseCardinal(
  //     new CardinalTraversal[A] {
  //       def execute[N0 <: Nat](n0 : N0, cn : CardinalNesting[N0, A]) : Option[CardinalNesting[N0, A]] = 
  //         getFlag(n0, k).extrudeDispatch(a0, a1, msk, ca, cn)
  //     }
  //   )(c)
  // }

}
