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

import TypeLemmas._
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

      ap[Lambda[`+B` => Tree[B, N]],
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
        inverseOf(
          treeSeqAssoc[A, _0, P, P, _0](lteRefl(p))
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
      inverseOf(cardinalTreeIsSeq(lte.lower))
    )

  }

  def cardinalTreeAssocInv[A, K <: Nat, N <: Nat, D <: Nat](lte : Lte[K, N, D]) 
      : CardinalTree[TreeSeq[A, S[K], D], K] === CardinalTree[A, S[N]] = 
    inverseOf(cardinalTreeAssoc[A, K, N, D](lte))

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
  // TO NESTING
  //

  @natElim
  def toPolarityNesting[A, N <: Nat](n: N)(cn: CardinalNesting[A, N]) : Nesting[Polarity[A], N] = {
    case (Z, Pt(nst)) => Box(Positive(), Pt(Nesting.map(nst)(Neutral(_))))
    case (S(p: P), cn) => {

      val polarizedNesting: CardinalNesting[Polarity[A], S[P]] = 
        mapCardinalTree[Nesting[A, S[P]], Nesting[Polarity[A], S[P]], S[P]](S(p))(cn)(Nesting.map(_)(Neutral(_)))

      Box(Positive(), Node(Dot(Negative(), S(p)), toShell(p)(polarizedNesting)))

    }
  }

  //============================================================================================
  // TO NESTING
  //

  @natElim
  def toNesting[A, N <: Nat](n: N)(cn: CardinalNesting[A, N], src: A, tgt: A) : Nesting[A, N] = {
    case (Z, cn, src, tgt) => Box(tgt, cn)
    case (S(p: P), cn, src, tgt) => Box(tgt, Node(Dot(src, S(p)), toShell[Nesting[A, S[P]], P](p)(cn)))
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
  // PASTE TO CARDINAL
  //

   def pasteToCardinal[A[_ <: Nat], N <: Nat](n: N)(pd: Tree[Complex[A, S[N]], S[N]])(disc: Discriminator[A]) 
    : ShapeM[Cardinal[A, S[N]]] = 
    for {
      pr <- Complex.paste(n)(pd)(disc)
    } yield {

      type INst[K <: Nat] = Nesting[A[K], K]

      val c0 = complexToCardinal(n)(pr._1)._1
      val c1 = mapCardinalTree[INst[N], Tree[Nesting[A[S[N]], S[N]], S[N]], N](n)(cardinalHead(c0))(_ => pr._2)

      c0 >> c1
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

  //============================================================================================
  // CARDINAL SPLIT/JOIN
  //

  case class CardinalSplit[A[_ <: Nat], K <: Nat, N <: Nat, D <: Nat](
    val lte: Lte[S[K], N, D],
    val prefix: Suite[Lambda[`M <: Nat` => CardinalNesting[A[M], M]], K],
    val focus: CardinalNesting[A[K], K],
    val next: CardinalNesting[A[S[K]], S[K]],
    val tail: Suite[Lambda[`M <: Nat` => CardinalNestingDblSucc[A[S[S[K#Plus[M]]]], K#Plus[M]]], D]
  )

  def cardinalSplit[A[_ <: Nat], K <: Nat, N <: Nat, D <: Nat](lte: Lte[S[K], N, D])(c: Cardinal[A, N]) : CardinalSplit[A, K, N, D] = {

    type INst[M <: Nat] = CardinalNesting[A[M], M]
    type SNst[M <: Nat, K <: Nat] = CardinalNesting[A[M#Plus[K]], M#Plus[K]]

    val (left, right) = Suite.grab[INst, D, S[N], S[S[K]]](lteInvert(SuccLte(lte)))(c)

    val next = Suite.head[INst, S[K]](left)
    val leftTail = Suite.tail[INst, S[K]](left)
    val focus = Suite.head[INst, K](leftTail)
    val prefix = Suite.tail[INst, K](leftTail)

    CardinalSplit[A, K, N, D](lte, prefix, focus, next, right)

  }

  def cardinalJoin[A[_ <: Nat], K <: Nat, N <: Nat, D <: Nat](split: CardinalSplit[A, K, N, D]) : Cardinal[A, N] = 
    split match {
      case CardinalSplit(lte, prefix, focus, next, tail) => {
        type INst[M <: Nat] = CardinalNesting[A[M], M]
        val ev: S[S[K]]#Plus[D] =::= S[N] = lteSumLemma(lteInvert(SuccLte(lte)))
        rewriteNatIn[Lambda[`M <: Nat` => Suite[INst, M]], S[S[K]]#Plus[D], S[N]](ev)(
          Suite.smash[INst, S[S[K]], D](S(lte.lower), lte.diff)(prefix >> focus >> next, tail)
        )
      }
    }

  //============================================================================================
  // SELECTION EXTRUSION
  //

  @natElim
  def extrudeAt[A, N <: Nat](n: N)(cn: CardinalNesting[A, N], ca: CardinalAddress[N], a: A)(pred: A => Boolean) : ShapeM[(CardinalNesting[A, N], Tree[Nesting[A, N], N])] = {
    case (Z, Pt(nst), ca, a, pred) => 
      if (pred(Nesting.baseValue(nst))) {  
        succeed(Pt(Box(a, Pt(nst))), Pt(nst))
      } else fail("Nothing selected")
    case (S(p: P), cn, tl >> hd, a, pred) => 
      for {
        pr <- poke(p)(cn, tl)
        zp <- Tree.seekTo(pr._1, hd)
        cut <- Tree.exciseWithProp(zp._1)((nst: Nesting[A, S[P]]) => pred(Nesting.baseValue(nst)))
      } yield {
        (plugCardinal(p)(pr._2, Zipper.close(S(p))(zp._2, Node(Box(a, cut._1), cut._2))), cut._1)
      }
  }

  @natElim
  def extrudeFillerAt[A, B, N <: Nat](n: N)(cn: CardinalNesting[A, S[N]], ca: CardinalAddress[N], msk: Tree[B, N], a: A) : ShapeM[CardinalNesting[A, S[N]]] = {
    case (Z, cn, ca, msk, a) => succeed(Pt(Node(Dot(a, __1), cn)))
    case (S(p), cn, tl >> hd, msk, a) =>
      for {
        pr <- poke(p)(cn, tl)
        zp <- Tree.seekTo(pr._1, hd)
        cut <- Tree.exciseWithMask(zp._1, msk)
      } yield {
        plugCardinal(p)(pr._2, Zipper.close(S(p))(zp._2, Node(Node(Dot(a, S(S(p))), cut._1), cut._2)))
      }
  }

  @lteElim
  def extrudeLeafAt[A, B, K <: Nat, N <: Nat, D <: Nat](lte: Lte[S[S[K]], N, D])(
    cn: CardinalNesting[A, N], ca: CardinalAddress[K], msk: Tree[B, K]
  ) : ShapeM[CardinalNesting[A, N]] = {
    case (SuccLte(SuccLte(ZeroLte(n0: N0))), cn, ca, msk) => 
      for {
        pr <- tailWithDerivative[Nesting[A, S[S[N0]]], _0, S[N0], S[N0]](cn, ca)(ZeroLte(S(n0)))
      } yield {
        symm[Nothing, Any, CardinalNestingDblSucc[A, N0], TreeSeqDblSucc[Nesting[A, S[S[N0]]], _0, N0]](
          cardinalTreeAssoc(SuccLte(ZeroLte(n0)))
        )(
          plugCardinal(__0)(pr._2, Node(seqLeaf(__1, n0), Pt(pr._1)))
        )
      }
    case (SuccLte(SuccLte(SuccLte(plte: (K0, N0, D0)))), cn, tl >> hd, msk) => {

      val k0 = plte.lower
      val ev : Lte[K0, S[S[N0]], S[S[D0]]] = lteSucc(lteSucc(plte))

      for {
        pr0 <- tailWithDerivative[Nesting[A, S[S[S[N0]]]], K0, S[S[N0]], S[S[D0]]](cn, tl)(ev)
        pr1 <- Tree.seekTo(pr0._1, hd)
        pr2 <- Tree.exciseWithMask(pr1._1, msk)
      } yield {
        symm[Nothing, Any, CardinalNestingTrplSucc[A, N0], CardinalTree[TreeSeq[Nesting[A, S[S[S[N0]]]], S[K0], S[S[D0]]], K0]](
          cardinalTreeAssoc[Nesting[A, S[S[S[N0]]]], K0, S[S[N0]], S[S[D0]]](ev)
        )(
          plugCardinal(k0)(pr0._2,
            Zipper.close(S(k0))(pr1._2, Node(Node(seqLeaf(S(S(k0)), plte.diff), pr2._1), pr2._2))
          )
        )
      }
    }
  }

  def extrudeSelection[A[_ <: Nat], K <: Nat, N <: Nat, D <: Nat](lte: Lte[S[K], N, D])(
    c: Cardinal[A, N], ca: CardinalAddress[K],
    a0: A[K], a1: A[S[K]]
  )(
    pred: A[K] => Boolean
  ): ShapeM[Cardinal[A, N]] = {

    type TailNst[M <: Nat] = CardinalNestingDblSucc[A[S[S[K#Plus[M]]]], K#Plus[M]]

    val k = lte.lower.pred
    val cs = cardinalSplit(lte)(c)

    for {
      pr <- extrudeAt(k)(cs.focus, ca, a0)(pred)
      newNext <- extrudeFillerAt(k)(cs.next, ca, pr._2, a1)
      newTail <- Suite.traverse[ShapeM, TailNst, TailNst, D](cs.tail)(
        new IndexedTraverse[ShapeM, TailNst, TailNst] {
          def apply[M <: Nat](m: M)(tnst: TailNst[M]) : ShapeM[TailNst[M]] = {
            extrudeLeafAt(SuccLte(SuccLte(ltePlusLemma(k)(m))))(tnst, ca, pr._2)
          }
        }
      )
    } yield cardinalJoin(CardinalSplit(lte, cs.prefix, pr._1, newNext, newTail))

  }

  //============================================================================================
  // DROP EXTRUSION
  //

  @natElim
  def extrudeLoopAt[A, N <: Nat](n: N)(cn: CardinalNesting[A, S[N]], ca: CardinalAddress[N], a: A) : ShapeM[CardinalNesting[A, S[N]]] = {
    case (Z, cn, ca, a) => succeed(Pt(Node(Box(a, Leaf(__1)), cn)))
    case (S(p: P), cn, ca, a) => {

      type T = Tree[Nesting[A, S[S[P]]], S[S[P]]]

      for {
        pr <- poke[T, S[P]](S(p))(cn, ca)
      } yield {
        plugCardinal[T, S[P]](S(p))(pr._2,
          Node(Box(a, Leaf(S(S(p)))), Node(pr._1, Tree.const(pr._2._2._1, Leaf(S(p)))))
        )
      }
    }
  }

  @natElim
  def extrudeDropAt[A, N <: Nat](n: N)(cn: CardinalNestingDblSucc[A, N], ca: CardinalAddress[N], a: A) : ShapeM[CardinalNestingDblSucc[A, N]] = {
    case (Z, cn, ca, a) => succeed(Pt(Node(Node(Dot(a, __2), Leaf(__1)), cn)))
    case (S(p: P), cn, ca, a) => {

      type T = Tree[Tree[Nesting[A, S[S[S[P]]]], S[S[S[P]]]], S[S[P]]]

      for {
        pr <- poke[T, S[P]](S(p))(cn, ca)
      } yield {
        plugCardinal[T, S[P]](S(p))(pr._2,
          Node(Node(Dot(a, S(S(S(p)))), Leaf(S(S(p)))), Node(pr._1, Tree.const(pr._2._2._1, Leaf(S(p)))))
        )
      }
    }
  }

  @lteElim
  def extrudeDropLeafAt[A, K <: Nat, N <: Nat, D <: Nat](lte: Lte[S[S[S[K]]], N, D])(
    cn: CardinalNesting[A, N], ca: CardinalAddress[K]
  ) : ShapeM[CardinalNesting[A, N]] = {
    case (SuccLte(SuccLte(SuccLte(ZeroLte(n0: N0)))), cn, ca) => {
      for {
        pr <- tailWithDerivative[Nesting[A, S[S[S[N0]]]], _0, S[S[N0]], S[S[N0]]](cn, ca)(ZeroLte(S(S(n0))))
      } yield {
        symm[Nothing, Any, CardinalNestingTrplSucc[A, N0], TreeSeqTrplSucc[Nesting[A, S[S[S[N0]]]], _0, N0]](
          cardinalTreeAssoc[Nesting[A, S[S[S[N0]]]], _2, S[S[N0]], N0](SuccLte(SuccLte(ZeroLte(n0))))
        )(
          plugCardinal(__0)(pr._2, Node(Node(seqLeaf(__2, n0), Leaf(__1)), Pt(pr._1)))
        )
      }
    }
    case (SuccLte(SuccLte(SuccLte(SuccLte(plte: (K0, N0, D0))))), cn, ca) => {

      val k0 : K0 = plte.lower
      val n0 : N0 = plte.upper
      val d0 : D0 = plte.diff

      val ev : Lte[S[K0], S[S[S[N0]]], S[S[D0]]] = lteSucc(lteSucc(SuccLte(plte)))

      for {
        pr <- tailWithDerivative[Nesting[A, S[S[S[S[N0]]]]], S[K0], S[S[S[N0]]], S[S[D0]]](cn, ca)(ev)
      } yield {
        symm[Nothing, Any, CardinalNestingQuadSucc[A, N0], CardinalTree[TreeSeq[Nesting[A, S[S[S[S[N0]]]]], S[S[K0]], S[S[D0]]], S[K0]]](
          cardinalTreeAssoc[Nesting[A, S[S[S[S[N0]]]]], S[K0], S[S[S[N0]]], S[S[D0]]](ev)
        )(
          plugCardinal(S(k0))(pr._2, Node(Node(seqLeaf(S(S(S(k0))), d0), Leaf(S(S(k0)))), Node(pr._1, Tree.const(pr._2._2._1, Leaf(S(k0))))))
        )
      }

    }
  }

  def dropAtAddress[A[_ <: Nat], K <: Nat, N <: Nat, D <: Nat](lte: Lte[S[S[K]], N, D])(
    c: Cardinal[A, N], ca: CardinalAddress[K],
    a0: A[S[K]], a1: A[S[S[K]]]
  ): ShapeM[Cardinal[A, N]] = {

    type TailNst[M <: Nat] = CardinalNestingTrplSucc[A[S[S[S[K#Plus[M]]]]], K#Plus[M]]

    val k = lte.lower.pred.pred
    val cs = cardinalSplit(lte)(c)

    for {
      newFcs <- extrudeLoopAt(k)(cs.focus, ca, a0)
      newNext <- extrudeDropAt(k)(cs.next, ca, a1)
      newTail <- Suite.traverse[ShapeM, TailNst, TailNst, D](cs.tail)(
        new IndexedTraverse[ShapeM, TailNst, TailNst] {
          def apply[M <: Nat](m: M)(tnst: TailNst[M]) : ShapeM[TailNst[M]] = {
            extrudeDropLeafAt(SuccLte(SuccLte(SuccLte(ltePlusLemma(k)(m)))))(tnst, ca)
          }
        }
      )
    } yield cardinalJoin(
      CardinalSplit[A, S[K], N, D](lte, cs.prefix, newFcs, newNext, newTail)
    )
  }

  //============================================================================================
  // SPROUT EXTRUSION
  //

  @natElim
  def sproutAt[A, N <: Nat](n: N)(cn: CardinalNesting[A, N], ca: CardinalAddress[S[N]], a: A) : ShapeM[CardinalNesting[A, N]] = {
    case (Z, Pt(nst), (tl >> hd), a) => {
      for {
        zp <- Nesting.seekNesting(__0)((nst, Nil), hd)
        res <- (
          zp._1 match {
            case Obj(a1) => succeed(Pt(Nesting.closeNesting(__0)(zp._2, Box(a1, Pt(Obj(a))))))
            case Box(a1, cn) => fail("Cannot sprout from box")
          }
        )
      } yield res
    }
    case (S(p: P), cn, tl >> hd, a) => {
      for {
        pr0 <- poke[Nesting[A, S[P]], S[P]](S(p))(cn, tl)
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
            case (Box(_, _), _) => fail("Cannot sprout from box")
          }
        )
      } yield res
    }
  }

  @natElim
  def sproutFillerAt[A, N <: Nat](n: N)(cn: CardinalNesting[A, S[N]], ca: CardinalAddress[S[N]], a: A) : ShapeM[(CardinalNesting[A, S[N]], Address[S[N]])] = {
    case (Z, cn, tl >> hd, a) => {
      for {
        pr0 <- poke(__0)(cn, tl)
        addrNst <- Nesting.canopyAddressExtend(__0)(pr0._1)
        pr1 <- Nesting.seekNesting(__0)((addrNst, Nil), hd)
        res <- (
          pr1._1 match {
            case Obj(addr) => {
              for {
                pr2 <- Tree.seekTo(pr0._1, addr)
                res1 <- (
                  pr2._1 match {
                    case Leaf(_) => succeed(
                      plugCardinal(__0)(pr0._2, Zipper.close(__1)(pr2._2, Node(Dot(a, __1), Pt(Leaf(__1)))))
                    )
                    case Node(_, _) => fail("Expected a leaf")
                  }
                )
              } yield (res1, addr)
            }
            case Box(_, _) => fail("Could not calculate modified address")
          }
        )
      } yield res
    }
    case (S(p: P), cn, tl >> hd, a) => {
      for {
        pr0 <- poke[Tree[Nesting[A, S[S[P]]], S[S[P]]], S[P]](S(p))(cn, tl)
        addrNst <- Nesting.canopyAddressExtend(S(p))(pr0._1)
        pr1 <- Nesting.seekNesting(S(p))((addrNst, Nil), hd)
        res <- (
          pr1._1 match {
            case Dot(addr, _) => {
              for {
                pr2 <- Tree.seekTo(pr0._1, addr)
                res1 <- (
                  pr2._1 match {
                    case Leaf(_) => {
                      val sh =
                        (pr2._2.headOption map ((c : (_, Derivative[_, S[P]])) => Tree.const(c._2._1, Leaf(S(p))))).
                          getOrElse(Tree.const(pr0._2._2._1, Leaf(S(p))))

                      succeed(
                        plugCardinal(S(p))(pr0._2, Zipper.close(S(S(p)))(pr2._2, Node(Dot(a, S(S(p))), Node(Leaf(S(S(p))), sh))))
                      )
                    }
                    case Node(_, _) => fail("Expected a leaf")
                  }
                )
              } yield (res1, addr)
            }
            case Box(_, _) => fail("Could not calculate modified address")
          }
        )
      } yield res
    }
  }

  @lteElim
  def sproutLeafAt[A, K <: Nat, N <: Nat, D <: Nat](lte: Lte[S[S[K]], N, D])(
    cn: CardinalNesting[A, N], ca: CardinalAddress[S[K]]
  ) : ShapeM[CardinalNesting[A, N]] = {
    case (SuccLte(SuccLte(ZeroLte(n0: N0))), cn, tl >> hd) => {
      for {
        pr0 <- tailWithDerivative[Nesting[A, S[S[N0]]], _0, S[N0], S[N0]](cn, tl)(ZeroLte(S(n0)))
        pr1 <- Tree.seekTo(pr0._1, hd)
        res <- (
          pr1._1 match {
            case Leaf(_) => {
              succeed(
                symm[Nothing, Any, CardinalNestingDblSucc[A, N0], TreeSeqDblSucc[Nesting[A, S[S[N0]]], _0, N0]](
                  cardinalTreeAssoc(SuccLte(ZeroLte(n0)))
                )(
                  plugCardinal(__0)(pr0._2, Zipper.close(__1)(pr1._2, Node(seqLeaf(__1, n0), Pt(Leaf(__1)))))
                )
              )
            }
            case Node(_, _) => fail("Expected a leaf")
          }
        )
      } yield res
    }
    case (SuccLte(SuccLte(SuccLte(plte: (K0, N0, D0)))), cn, tl >> hd) => {

      val k0 : K0 = plte.lower
      val ev : Lte[S[K0], S[S[N0]], S[D0]] = SuccLte(lteSucc(plte))

      for {
        pr0 <- tailWithDerivative[Nesting[A, S[S[S[N0]]]], S[K0], S[S[N0]], S[D0]](cn, tl)(ev)
        pr1 <- Tree.seekTo(pr0._1, hd)
        res <- (
          pr1._1 match {
            case Leaf(_) => {
              val sh =
                (pr1._2.headOption map ((c : (_, Derivative[_, S[K0]])) =>Tree.const(c._2._1, Leaf(S(k0))))).
                  getOrElse(Tree.const(pr0._2._2._1, Leaf(S(k0))))

              succeed(
                symm[Nothing, Any, CardinalNestingTrplSucc[A, N0], CardinalTree[TreeSeq[Nesting[A, S[S[S[N0]]]], S[S[K0]], S[D0]], S[K0]]](
                  cardinalTreeAssoc[Nesting[A, S[S[S[N0]]]], S[K0], S[S[N0]], S[D0]](ev)
                )(
                  plugCardinal(S(k0))(pr0._2, Zipper.close(S(S(k0)))(pr1._2, Node(seqLeaf(S(S(k0)), plte.diff), Node(Leaf(S(S(k0))), sh))))
                )
              )
            }
            case Node(_, _) => fail("Expected a leaf, damnit!")
          }
        )
      } yield res
    }
  }

  def sproutAtAddress[A[_ <: Nat], K <: Nat, N <: Nat, D <: Nat](lte: Lte[S[K], N, D])(
    c: Cardinal[A, N], ca: CardinalAddress[S[K]],
    a0: A[K], a1: A[S[K]]
  ): ShapeM[Cardinal[A, N]] = {

    type TailNst[M <: Nat] = CardinalNestingDblSucc[A[S[S[K#Plus[M]]]], K#Plus[M]]

    val k = lte.lower.pred
    val cs = cardinalSplit(lte)(c)

    for {
      newFcs <- sproutAt(k)(cs.focus, ca, a0)
      pr <- sproutFillerAt(k)(cs.next, ca, a1)
      newTail <- Suite.traverse[ShapeM, TailNst, TailNst, D](cs.tail)(
        new IndexedTraverse[ShapeM, TailNst, TailNst] {
          def apply[M <: Nat](m: M)(tnst: TailNst[M]) : ShapeM[TailNst[M]] = {
            sproutLeafAt(SuccLte(SuccLte(ltePlusLemma(k)(m))))(tnst, Suite.tail(ca) >> pr._2)
          }
        }
      )
    } yield cardinalJoin(CardinalSplit(lte, cs.prefix, newFcs, pr._1, newTail))
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
