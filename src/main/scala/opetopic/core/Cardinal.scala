/**
  * Cardinal.scala - Opetopic Cardinals
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Leibniz
import scalaz.Leibniz._

import Nat._
import Zippers._
import Complex._

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

  type TreeSeqSucc[N <: Nat, P <: Nat, +A] = 
    Tree[N, TreeSeq[S[N], P, A]]
  type TreeSeqDblSucc[N <: Nat, P <: Nat, +A] = 
    Tree[N, Tree[S[N], TreeSeq[S[S[N]], P, A]]]
  type TreeSeqTrplSucc[N <: Nat, P <: Nat, +A] = 
    Tree[N, Tree[S[N], Tree[S[S[N]], TreeSeq[S[S[S[N]]], P, A]]]]
  type TreeSeqQuadSucc[N <: Nat, P <: Nat, +A] = 
    Tree[N, Tree[S[N], Tree[S[S[N]], Tree[S[S[S[N]]], TreeSeq[S[S[S[S[N]]]], P, A]]]]]

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
  type CardinalTreeTrplSucc[P <: Nat, +A] =
    CardinalTree[P, Tree[S[P], Tree[S[S[P]], Tree[S[S[S[P]]], A]]]]
  type CardinalTreeQuadSucc[P <: Nat, +A] = 
    CardinalTree[P, Tree[S[P], Tree[S[S[P]], Tree[S[S[S[P]]], Tree[S[S[S[S[P]]]], A]]]]]

  type CardinalNesting[N <: Nat, +A] = CardinalTree[N, Nesting[N, A]]
  type CardinalNestingSucc[P <: Nat, +A] = 
    CardinalTreeSucc[P, Nesting[S[P], A]]
  type CardinalNestingDblSucc[P <: Nat, +A] = 
    CardinalTreeDblSucc[P, Nesting[S[S[P]], A]]
  type CardinalNestingTrplSucc[P <: Nat, +A] = 
    CardinalTreeTrplSucc[P, Nesting[S[S[S[P]]], A]]
  type CardinalNestingQuadSucc[P <: Nat, +A] = 
    CardinalTreeQuadSucc[P, Nesting[S[S[S[S[P]]]], A]]

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
  // MAP IMPLEMENTATIONS
  //

  def mapCardinalTree[N <: Nat, A, B](n : N)(ct : CardinalTree[N, A])(f : A => B) : CardinalTree[N, B] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = CardinalTree[N, A] => CardinalTree[N, B]

      def caseZero : Out[_0] = {
        case Pt(a) => Pt(f(a))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        cts => mapCardinalTree(p)(cts)(Tree.map(_)(f))
      }

    })(n)(ct)

  def mapCardinalTreeWithAddr[N <: Nat, A, B](n : N)(ct : CardinalTree[N, A])(f : (CardinalAddress[N], A) => B) : CardinalTree[N, B] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = (CardinalTree[N, A], (CardinalAddress[N], A) => B) => CardinalTree[N, B]

      def caseZero : Out[_0] = {
        case (Pt(a), f) => Pt(f(TNil[Address]() >> (()), a))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (ct, f) => {
          mapCardinalTreeWithAddr(p)(ct)({
            case (ca, tr) => Tree.mapWithAddress(tr)({
              case (addr, a) => f(ca >> addr, a)
            })
          })
        }
      }

    })(n)(ct, f)

  def mapCardinalNesting[N <: Nat, A, B](n : N)(cn : CardinalNesting[N, A])(f : A => B) : CardinalNesting[N, B] = 
    mapCardinalTree(n)(cn)(Nesting.mapNesting(_)(f))

  def mapCardinalNestingWithAddr[N <: Nat, A, B](n : N)(cn : CardinalNesting[N, A])(f : (CardinalAddress[S[N]], A) => B) : CardinalNesting[N, B] = 
    mapCardinalTreeWithAddr(n)(cn)({
      (ca, nst) => nst.mapWithAddress({
        case (dir, a) => f(ca >> dir, a)
      })
    })

  def mapCardinal[N <: Nat, A, B](c : Cardinal[N, A])(f : A => B) : Cardinal[N, B] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = Cardinal[N, A] => Cardinal[N, B]

      def caseZero : Out[_0] = {
        case (_ >>> hd) => CNil() >>> mapCardinalNesting(Z)(hd)(f)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (tl >>> hd) => mapCardinal(tl)(f) >>> mapCardinalNesting(S(p))(hd)(f)
      }

    })(c.dim)(c)

  //============================================================================================
  // COMPLETE WITH
  //

  def completeWith[N <: Nat, A](n : N)(a : A, ct : CardinalTree[N, A]) : Tree[N, A] =
    (new NatCaseSplit {
      
      type Out[N <: Nat] = CardinalTree[N, A] => Tree[N, A]

      def caseZero : Out[_0] = {
        ct => ct
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        ct => Node(a, completeWith(p)(Leaf(S(p)), ct))
      }

    })(n)(ct)

  def toShell[N <: Nat, A](n : N)(ct : CardinalTree[S[N], A]) : Tree[N, Tree[S[N], A]] = 
    completeWith(n)(Leaf(S(n)), ct)

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

  def extendCardinal[N <: Nat, A](c : Cardinal[N, A])(f : CardinalAddress[S[N]] => A) : Cardinal[S[N], A] = 
    c match {
      case (tl >>> hd) => {
        val extension = mapCardinalTreeWithAddr(c.dim)(hd)({ 
          case (ca, nst) => Nesting.newNestingExtend(nst)({ 
            case addr => f(ca >> addr)
          })
        })

        tl >>> hd >>> extension
      }
    }

  //============================================================================================
  // ROOT ADDRESS
  //

  object cardinalRootAddr extends NatCaseSplit0 {

    type Out[N <: Nat] = CardinalAddress[N]

    def caseZero : Out[_0] = TNil[Address]() >> Nil

    def caseSucc[P <: Nat](p : P) : Out[S[P]] =
      cardinalRootAddr(p) >> Nil

  }

  object cardinalRootTree extends NatCaseSplit1 {

    type Out[N <: Nat, A] = CardinalNesting[N, A] => Option[Tree[N, Nesting[N, A]]]

    def caseZero[A] : Out[_0, A] = 
      cn => Some(cn)

    def caseSucc[P <: Nat, A](p : P) : Out[S[P], A] = 
      cn => for {
        res <- poke(cn, cardinalRootAddr(p))
      } yield res._2

  }

  //============================================================================================
  // TO COMPLEX
  //

  sealed trait Polarity[A]
  sealed trait Polarization[A] extends Polarity[A]
  case class Positive[A]() extends Polarization[A] { override def toString = "+" }
  case class Negative[A]() extends Polarization[A] { override def toString = "-" }
  case class Neutral[A](a : A) extends Polarity[A] { override def toString = a.toString }

  trait CardinalCellGenerator[F[_], A] {

    def positive[N <: Nat](n : N) : F[A]
    def negative[N <: Nat](n : N) : F[A]

    def neutral[N <: Nat](a : A) : F[A]

  }

  def toComplex[F[_], N <: Nat, A](c : Cardinal[N, A])(gen : CardinalCellGenerator[F, A]) : Complex[N, F[A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = Cardinal[N, A] => Complex[N, F[A]]

      def caseZero : Out[_0] = {
        case (_ >>> Pt(nst)) => CNil() >>> Box(gen.positive(Z), Pt(nst map (gen.neutral(_))))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (tl >>> hd) => {
          val shell = toShell(p)(mapCardinalNesting(S(p))(hd)(gen.neutral(_)))
          toComplex(tl)(gen) >>> Box(gen.positive(S(p)), Node(Dot(gen.negative(S(p)), S(p)), shell))
        }
      }

    })(c.dim)(c)

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

  def encloseAt[N <: Nat, A, B](a : A, addr : Address[S[N]], tr : Tree[S[N], Tree[S[S[N]], A]], msk : Tree[S[N], B]) : Option[Tree[S[N], Tree[S[S[N]], A]]] = 
    for {
      zp <- tr seekTo addr
      (cut, cutSh) <- Tree.excise(zp.focus, msk)
    } yield close(tr.dim)(zp.context, 
      Node(Node(a, cut), cutSh)
    )

  //============================================================================================
  // PAD WITH LEAF
  //

  def padWithLeaf[N <: Nat, K <: Nat, A, B](k : K, addr : Address[S[N]], seq : TreeSeqDblSucc[S[N], K, A], msk : Tree[S[N], B]) : Option[TreeSeqDblSucc[S[N], K, A]] =
    for {
      zp <- seq seekTo addr 
      (cut, cutSh) <- Tree.excise(zp.focus, msk)
    } yield close(seq.dim)(zp.context,
      Node(Node(seqLeaf(S(seq.dim), k), cut), cutSh)
    )

  //============================================================================================
  // DO TAIL
  //

  def doTail[K <: Nat, N <: Nat, D <: Nat, A, B](lte : Lte[K, N, D], cn : CardinalNestingDblSucc[N, A], ca : CardinalAddress[K], msk : Tree[K, B]) 
      : Option[CardinalNestingDblSucc[N, A]] = 
    (new LteCaseSplit {

      type Out[K <: Nat, N <: Nat, D <: Nat] = (CardinalNestingDblSucc[N, A], CardinalAddress[K], Tree[K, B]) => Option[CardinalNestingDblSucc[N, A]]

      def caseZero[N <: Nat](n : N) : Out[_0, N, N] = {
        case (cn, ca, msk) => 
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
        case (cn, ca >> addr, msk) => {

          import Lte.lteSucc

          type SeqType = TreeSeqDblSucc[S[K], D, Nesting[S[S[S[N]]], A]]
          type DerivType = CardinalDerivative[K, SeqType]

          for {
            pr <- tailWithDerivative[S[S[N]], K, S[S[D]], Nesting[S[S[S[N]]], A]](cn, ca)(lteSucc(lteSucc(plte)))
            res <- padWithLeaf(plte.diff, addr, pr._2, msk)
          } yield {

            type SrcType = CardinalTree[N, Tree[S[N], Tree[S[S[N]], Tree[S[S[S[N]]], Nesting[S[S[S[N]]], A]]]]]
            type TgtType = CardinalTree[K, Tree[S[K], Tree[S[S[K]], TreeSeq[S[S[S[K]]], D, Nesting[S[S[S[N]]], A]]]]]

            symm[Nothing, Any, SrcType, TgtType](
              cardinalTreeAssoc[S[S[N]], S[S[K]], D, Nesting[S[S[S[N]]], A]](SuccLte(SuccLte(plte)))
            )(
              plugCardinal(plte.lower)(pr._1, res)
            )
          }
        }
      }

    })(lte)(cn, ca, msk)

  //============================================================================================
  // DO FILLER
  //

  def doFiller[N <: Nat, A, B](a : A, cn : CardinalNesting[S[N], A], ca : CardinalAddress[N], msk : Tree[N, B]) : Option[CardinalNesting[S[N], A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = (CardinalNestingSucc[N, A], CardinalAddress[N], Tree[N, B]) => Option[CardinalNestingSucc[N, A]]

      def caseZero : Out[_0] = {
        case (cn, _ >> addr, msk) => Some(Pt(Node(Dot(a, S(Z)), cn)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (cn, ca >> hdAddr, msk) => {

          type SeqType = TreeSeq[S[P], _0, Tree[S[S[P]], Nesting[S[S[P]], A]]]
          type DerivType = CardinalDerivative[P, SeqType]

          for {
            pr <- tailWithDerivative[P, P, _0, Tree[S[S[P]], Nesting[S[S[P]], A]]](cn, ca)(Lte.lteRefl(ca.length.pred))
            res <- encloseAt[P, Nesting[S[S[P]], A], B](Dot(a, S(ca.length)), hdAddr, pr._2, msk)
          } yield {
            plugCardinal[P, SeqType](p)(pr._1, res)
          }
        }
      }

    })(ca.length.pred)(cn, ca, msk)

  //============================================================================================
  // EXTRUDE LOOP AT
  //

  object extrudeLoopAt extends NatCaseSplit1 {

    type Out[N <: Nat, A] = (A, CardinalNesting[S[N], A], CardinalAddress[N]) => Option[CardinalNesting[S[N], A]]

    def caseZero[A] : Out[_0, A] = {
      case (a, cn, ca) => Some(Pt(Node(Box(a, Leaf(__1)), cn)))
    }

    def caseSucc[P <: Nat, A](p : P) : (A, CardinalNestingDblSucc[P, A], CardinalAddress[S[P]]) => Option[CardinalNestingDblSucc[P, A]] = {
      case (a, cn, ca) => 
        for {
          (deriv, tr) <- poke[S[P], Tree[S[S[P]], Nesting[S[S[P]], A]]](cn, ca)
        } yield {
          plugCardinal[S[P], Tree[S[S[P]], Nesting[S[S[P]], A]]](S(p))(deriv, 
            Node(Box(a, Leaf(S(S(p)))), Node(tr, deriv._2._1.constWith(Leaf(S(p)))))
          )
        }
    }

  }

  //============================================================================================
  // EXTRUDE DROP AT
  //

  object extrudeDropAt extends NatCaseSplit1 {

    type Out[N <: Nat, A] = (A, CardinalNesting[S[S[N]], A], CardinalAddress[N]) => Option[CardinalNesting[S[S[N]], A]]

    def caseZero[A] : (A, CardinalNestingDblSucc[_0, A], CardinalAddress[_0]) => Option[CardinalNestingDblSucc[_0, A]] = {
      case (a, cn, ca) => Some(Pt(Node(Node(Dot(a, __2), Leaf(__1)), cn)))
    }

    def caseSucc[P <: Nat, A](p : P) : (A, CardinalNestingTrplSucc[P, A], CardinalAddress[S[P]]) => Option[CardinalNestingTrplSucc[P, A]] = {
      case (a, cn, ca) => 
        for {
          (deriv, tr) <- poke[S[P], Tree[S[S[P]], Tree[S[S[S[P]]], Nesting[S[S[S[P]]], A]]]](cn, ca)
        } yield {
          plugCardinal[S[P], Tree[S[S[P]], Tree[S[S[S[P]]], Nesting[S[S[S[P]]], A]]]](S(p))(deriv, 
            Node(Node(Dot(a, S(S(S(p)))), Leaf(S(S(p)))), Node(tr, deriv._2._1.constWith(Leaf(S(p)))))
          )
        }
    }

  }

  //============================================================================================
  // PAD WITH DROP LEAF
  //

  // What a horrendous nightmare.  Please rewrite this to make it readable ...

  def padWithDropLeaf[A, K <: Nat, N <: Nat, D <: Nat](cn : CardinalNesting[N, A], ca : CardinalAddress[K])(lte : Lte[S[S[S[K]]], N, D]) 
      : Option[CardinalNesting[N, A]] = 
    (new NatCaseSplitWithOne { sp => 

      type Out[N <: Nat] = (Lte[S[S[S[K]]], N, D], CardinalNesting[N, A], CardinalAddress[K]) => Option[CardinalNesting[N, A]]

      def caseZero : Out[_0] = {
        case (_, _, _) => None  // Unreachable
      }

      def caseOne : Out[_1] = {
        case (_, _, _) => None  // Unreachable
      }

      def caseDblSucc[P <: Nat](p : P) 
          : (Lte[S[S[S[K]]], S[S[P]], D], CardinalNestingDblSucc[P, A], CardinalAddress[K]) => Option[CardinalNestingDblSucc[P, A]]
      = {
        case (plte, cn, ca) => {
          (new NatCaseSplit {

            type Out[Q <: Nat] = sp.Out[S[S[Q]]]

            def caseZero : (Lte[S[S[S[K]]], S[S[_0]], D], CardinalNestingDblSucc[_0, A], CardinalAddress[K]) => Option[CardinalNestingDblSucc[_0, A]] = {
              case (_, _, _) => None  // Unreachable
            }

            def caseSucc[Q <: Nat](q : Q) : (Lte[S[S[S[K]]], S[S[S[Q]]], D], CardinalNestingTrplSucc[Q, A], CardinalAddress[K]) => Option[CardinalNestingTrplSucc[Q, A]] = {
              case (SuccLte(SuccLte(SuccLte(qlte))), cn, ca) => 
                (new LteCaseSplit {

                  type Out[K <: Nat, N <: Nat, D <: Nat] = (CardinalNestingTrplSucc[N, A], CardinalAddress[K]) => Option[CardinalNestingTrplSucc[N, A]]

                  def caseZero[N <: Nat](n : N) : Out[_0, N, N] = {
                    case (cn, ca) => 
                      for {
                        pr <- tailWithDerivative[S[S[N]], _0, S[S[N]], Nesting[S[S[S[N]]], A]](cn, ca)(ZeroLte(S(S(n))))
                      } yield { 
                        symm[Nothing, Any, CardinalNestingTrplSucc[N, A], TreeSeqTrplSucc[_0, N, Nesting[S[S[S[N]]], A]]](
                          cardinalTreeAssoc[S[S[N]], _2, N, Nesting[S[S[S[N]]], A]](SuccLte(SuccLte(ZeroLte(n))))
                        )(
                          plugCardinal(__0)(pr._1, Node(Node(seqLeaf(__2, n), Leaf(__1)), Pt(pr._2)))
                        )
                      }
                  }

                  def caseSucc[K <: Nat, N <: Nat, D <: Nat](plte : Lte[K, N, D]) : Out[S[K], S[N], D] = {
                    case (cn, ca) => {
                      import Lte._

                      val ev = lteSucc(lteSucc(SuccLte(plte)))

                      for {
                        pr <- tailWithDerivative[S[S[S[N]]],S[K],S[S[D]], Nesting[S[S[S[S[N]]]], A]](cn, ca)(ev)
                      } yield {

                        val k : K = plte.lower
                        val n : N = plte.upper
                        val d : D = plte.diff

                        symm[Nothing, Any, CardinalNestingQuadSucc[N, A], CardinalTree[S[K], TreeSeq[S[S[K]], S[S[D]], Nesting[S[S[S[S[N]]]], A]]]](
                          cardinalTreeAssoc[S[S[S[N]]], S[K], S[S[D]], Nesting[S[S[S[S[N]]]], A]](ev)
                        )(
                          plugCardinal(S(k))(pr._1, Node(Node(seqLeaf(S(S(S(k))), d), Leaf(S(S(k)))), Node(pr._2, pr._1._2._1.constWith(Leaf(S(k))))))
                        )
                      }
                    }
                  }

                })(qlte)(cn, ca)
            }

          })(p)(plte, cn, ca)
        }
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
  // EXTRUDE DISPATCH
  //

  def extrudeDispatch[N <: Nat, K <: Nat, A, B](flag : CardinalDimFlag[N, K])(
    a0 : A, a1 : A,
    msk : Tree[K, B],
    ca : CardinalAddress[K],
    cn : CardinalNesting[N, A]
  ) : Option[CardinalNesting[N, A]] = 
    (new DimDispatcher[K] {

      type Out[N <: Nat] = (Tree[K, B], CardinalAddress[K], CardinalNesting[N, A]) => Option[CardinalNesting[N, A]]

      def caseEq(k : K) : Out[K] = {
        case (msk, ca, cn) => {
          (new NatCaseSplit {

            type Out[N <: Nat] = (Tree[N, B], CardinalAddress[N], CardinalNesting[N, A]) => Option[CardinalNesting[N, A]]

            def caseZero : Out[_0] = {
              (msk, ca, cn) => Some(Pt(Box(a0, cn)))
            }

            def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
              (msk, ca, cn) => extrudeNestingAt(a0, cn, ca, msk)
            }

          })(k)(msk, ca, cn)
        }
      }


      def caseLt[N <: Nat, D <: Nat](slte : Lte[S[N], K, D]) : Out[N] = {
        case (msk, ca, cn) => Some(cn)
      }

      def caseSucc(k : K) : Out[S[K]] = {
        case (msk, ca, cn) => doFiller(a1, cn, ca, msk)
      }

      def caseDblSucc[N <: Nat, D <: Nat](sslte : Lte[S[S[K]], N, D]) : Out[N] = {
        case (msk, ca, cn) => 
          (new NatCaseSplitWithOne {

            type Out[N <: Nat] = (Lte[S[S[K]], N, D], CardinalNesting[N, A]) => Option[CardinalNesting[N, A]]

            def caseZero : Out[_0] = {
              case (_, _) => None  // Unreachable
            }

            def caseOne : Out[_1] = {
              case (_, _) => None // Unreachable
            }

            def caseDblSucc[P <: Nat](p : P) : (Lte[S[S[K]], S[S[P]], D], CardinalNestingDblSucc[P, A]) => Option[CardinalNestingDblSucc[P, A]] = {
              case (SuccLte(SuccLte(lte)), cn) => doTail(lte, cn, ca, msk)
            }

          })(sslte.upper)(sslte, cn)
      }

    })(flag)(msk, ca, cn)

  //============================================================================================
  // DROP DISPATCH
  //

  def dropDispatch[N <: Nat, K <: Nat, A](flag : CardinalDimFlag[N, S[K]])(
    a0 : A, a1 : A,
    ca : CardinalAddress[K],
    cn : CardinalNesting[N, A]
  ) : Option[CardinalNesting[N, A]] = 
    (new DimDispatcher[S[K]] {

      type Out[N <: Nat] = CardinalNesting[N, A] => Option[CardinalNesting[N, A]]

      def caseEq(sk : S[K]) : Out[S[K]] = {
        cn => extrudeLoopAt(sk.pred)(a0, cn, ca)
      }

      def caseLt[N <: Nat, D <: Nat](slte : Lte[S[N], S[K], D]) : Out[N] = {
        cn => Some(cn)
      }

      def caseSucc(sk : S[K]) : CardinalNestingDblSucc[K, A] => Option[CardinalNestingDblSucc[K, A]] = {
        cn => extrudeDropAt(sk.pred)(a1, cn, ca)
      }

      def caseDblSucc[N <: Nat, D <: Nat](sslte : Lte[S[S[S[K]]], N, D]) : Out[N] = {
        cn => padWithDropLeaf(cn, ca)(sslte)
      }

    })(flag)(cn)

  //============================================================================================
  // TRAVERSE CARDINAL
  //

  // Rewrite this using fold or something ....

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
          extrudeDispatch(getFlag(n0, k))(a0, a1, msk, ca, cn)
      }
    )(c)
  }

  def doDrop[N <: Nat, K <: Nat, A](
    a0 : A, a1 : A,
    ca : CardinalAddress[K],
    c : Cardinal[N, A]
  ) : Option[Cardinal[N, A]] = 
    traverseCardinal(new CardinalTraversal[A] {
      def apply[N <: Nat](n : N, cn : CardinalNesting[N, A]) : Option[CardinalNesting[N, A]] =
        dropDispatch(getFlag(n, S(ca.length.pred)))(a0, a1, ca, cn)
    })(c)

  def doRootExtrusion[K <: Nat, N <: Nat, D <: Nat, A](k : K)(a0 : A, a1 : A, c : Cardinal[S[N], A])(implicit lte : Lte[K, N, D]) : Option[Cardinal[S[N], A]] = 
    for {
      tr <- cardinalRootTree(lte.lower)(c.tail.getAt(lte))
      res <- doExtrude(a0, a1, tr, cardinalRootAddr(lte.lower), c)
    } yield res

  def doTopRootExtrusion[N <: Nat, A](a0 : A, a1 : A, c : Cardinal[N, A]) : Option[Cardinal[S[N], A]] = 
    doRootExtrusion(c.dim)(a0, a1, extendCardinal(c)(_ => a1))(Lte.lteRefl(c.dim))

  def getSelectionMask[N <: Nat, A](cn : CardinalNesting[S[N], A], ca : CardinalAddress[S[N]])(p : A => Boolean) : Option[Tree[S[N], Nesting[S[N], A]]] = 
    ca match {
      case (ca >> hdAddr) =>
        for {
          cd <- poke(cn, ca)
          zp <- cd._2 seekTo hdAddr
        } yield Tree.takeWhile(zp._1)(nst => p(nst.baseValue))
    }

  def extrudeSelection[K <: Nat, N <: Nat, D <: Nat, A](a0 : A, a1 : A, ca : CardinalAddress[K], c : Cardinal[N, A])(p : A => Boolean)(lte : Lte[K, N, D]) : Option[Cardinal[N, A]] = 
    (new LteCaseSplit {

      type Out[K <: Nat, N <: Nat, D <: Nat] = (CardinalAddress[K], Cardinal[N, A]) => Option[Cardinal[N, A]]

      def caseZero[N <: Nat](n : N) : Out[_0, N, N] =
        (ca, c) => doExtrude(a0, a1, Pt(()), ca, c)

      def caseSucc[K <: Nat, N <: Nat, D <: Nat](plte : Lte[K, N, D]) : Out[S[K], S[N], D] = 
        (ca, c) => {
          for {
            msk <- getSelectionMask(c.getAt(SuccLte(plte)), ca)(p)
            res <- doExtrude(a0, a1, msk, ca, c)
          } yield res
        }

    })(lte)(ca, c)

  //============================================================================================
  // OPS CLASS
  //

  implicit def cardinalToSigma[M <: Nat, A](c : Cardinal[M, A]) : Sigma[Cardinal, A] =
    new Sigma[Cardinal, A] {
      type N = M
      val n = c.dim
      val value = c
    }

  implicit def cardinalFromSigma[A](cs : Sigma[Cardinal, A]) : Cardinal[cs.N, A] = 
    cs.value

  class CardinalOps[N <: Nat, A](c : Cardinal[N, A]) {

    def dim : N = c.length.pred

    def toComplexWith[F[_]](gen : CardinalCellGenerator[F, A]) : Complex[N, F[A]] = 
      Cardinals.toComplex(c)(gen)

    def toComplex : Complex[N, Polarity[A]] = 
      Cardinals.toComplex(c)(new CardinalCellGenerator[Polarity, A] {

        def positive[N <: Nat](n : N) : Polarity[A] = Positive()
        def negative[N <: Nat](n : N) : Polarity[A] = Negative()

        def neutral[N <: Nat](a : A) : Polarity[A] = Neutral(a)

      })
  }


  // trait CardinalCellGenerator[F[_], A] {

  //   def positive[N <: Nat](n : N) : F[A]
  //   def negative[N <: Nat](n : N) : F[A]

  //   def neutral[N <: Nat](a : A) : F[A]

  // }

  implicit def cardinalToOps[N <: Nat, A](c : Cardinal[N, A]) : CardinalOps[N, A] = 
    new CardinalOps(c)

  implicit def cardinalSigmaToOps[A](cs : Sigma[Cardinal, A]) : CardinalOps[cs.N, A] = 
    new CardinalOps(cs.value)

}
