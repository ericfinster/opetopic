/**
  * ComplexZipper.scala - Zippers for Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scalaz.{Tree => _, Zipper => _, _}
import scalaz.std.option._

import Nat._
import Tree._
import Nesting._
import Complex._
import Zippers._

object ComplexZipper {

  type ComplexZipper[N <: Nat, +A] = ConsSeq[NestingZipper, S[N], A]

  //============================================================================================
  // SEAL
  //

  def seal[N <: Nat, A](cz : ComplexZipper[N, A]) : Complex[N, A] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = ComplexZipper[N, A] => Complex[N, A]

      def caseZero : Out[_0] = {
        case >>>(_, (nst, cntxt)) => CNil() >>> closeNesting(__0)(cntxt, nst)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case >>>(tl, (nst, cntxt)) => seal(tl) >>> closeNesting(S(p))(cntxt, nst)
      }

    })(cz.length.pred)(cz)

  //============================================================================================
  // FROM COMPLEX
  //

  def fromComplex[N <: Nat, A](cmplx : Complex[N, A]) : ComplexZipper[N, A] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = Complex[N, A] => ComplexZipper[N, A]

      def caseZero : Out[_0] = {
        case (_ >>> nst) => CNil[NestingZipper]() >>> (nst, Nil)
      }
        
      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (tl >>> nst) => fromComplex(tl) >>> (nst, Nil)
      }

    })(cmplx.dim)(cmplx)

  //============================================================================================
  // VISIT
  //

  def visitComplex[N <: Nat, A](dir : Address[N], cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = (Address[N], ComplexZipper[N, A]) => Option[ComplexZipper[N, A]]

      def caseZero : Out[_0] = {
        case (dir, _ >>> nst) => for { zp <- visitNesting(dir, nst) } yield CNil[NestingZipper] >>> zp
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (Nil, tl >>> nst) => for { zp <- visitNesting(Nil : Address[S[P]], nst) } yield tl >>> zp
        case (d :: ds, zipper) => 
          for {
            prefixZipper <- visitComplex[S[P], A](ds, zipper)
            nestingSibling <- sibling(d, prefixZipper.head)  
            prefixSpine <- focusSpine(prefixZipper)

            res <- (
              prefixSpine match {
                case Leaf(_) => Some(prefixZipper.tail >>> nestingSibling)
                case Node(_, shell) =>
                  for {
                    extents <- Tree.shellExtents(shell)
                    recAddr <- extents valueAt d 
                    fixupLower <- seekComplex(recAddr, prefixZipper.tail)
                  } yield (fixupLower >>> nestingSibling)
              }
            )
          } yield res
      }

    })(cz.length.pred)(dir, cz)

  //============================================================================================
  // SEEK
  //

  def seekComplex[N <: Nat, A](addr : Address[S[N]], cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] =
    addr match {
      case Nil => Some(cz)
      case (d :: ds) =>
        for {
          zp <- seekComplex(ds, cz)
          zr <- visitComplex(d, zp)
        } yield zr
    }

  //============================================================================================
  // FOCUS ROUTINES
  //

  def updateFocus[N <: Nat, A](cz : ComplexZipper[N, A], nst : Nesting[N, A]) : ComplexZipper[N, A] =
    cz match {
      case (tl >>> nz) => tl >>> (nst, nz._2)
    }

  def focusValue[N <: Nat, A](cz : ComplexZipper[N, A]) : A =
    baseValue(cz.head._1)

  def focusDeriv[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[Derivative[S[N], A]] = 
    cz match {
      case (tl >>> ((Obj(a), _))) => Some(Pt(Leaf(__1)), Nil)
      case (tl >>> ((Dot(a, d), _))) => None
      case (tl >>> ((Box(a, c), _))) => Some(c.constWith(Leaf(S(c.dim))), Nil)
    }

  def focusSpine[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[Tree[N, A]] = 
    cz match {
      case (tl >>> ((Obj(a), _))) => Some(Pt(a))
      case (tl >>> ((Dot(a, d), _))) => for { deriv <- focusDeriv(tl) } yield plug(d)(deriv, a)
      case (tl >>> ((Box(a, c), _))) => spineFromCanopy(c)
    }

  def focusCanopy[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[Tree[N, Address[N]]] =
    cz match {
      case (tl >>> ((Obj(a), _))) => Some(Pt(rootAddr(__0)))
      case (tl >>> ((Dot(a, d), _))) => None
      case (tl >>> ((Box(a, c), _))) => Some(c.addressTree)
    }

  // The idea here is to embed the current focus of this nesting zipping in exactly the
  // unit tree which surrounds it.  Notice that I think this calculation could be eliminated
  // if you were more careful in the compression routine to pass along the required derivative
  // as the tree was compressed.  Have to look into that ...

  def focusUnit[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[Tree[N, Nesting[N, A]]] =
    (new NatCaseSplit {

      type Out[N <: Nat] = ComplexZipper[N, A] => Option[Tree[N, Nesting[N, A]]]

      def caseZero : Out[_0] = 
        cz => Some(Pt(cz.head._1))

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = cz => {

        val fcs = cz.head._1

        for {
          spine <- focusSpine(cz)
          res <- (
            spine match {
              case Leaf(d) =>
                for {
                  unit <- focusUnit(cz.tail)
                } yield Node(fcs, unit.constWith(Leaf(d)))
              case Node(a, shell) =>
                for {
                  extents <- shellExtents(shell)
                } yield Node(fcs, extents.constWith(Leaf(S(shell.dim))))
            }
          )
        } yield res

      }

    })(cz.length.pred)(cz)

  //============================================================================================
  // RESTRICT FOCUS
  //

  def restrictFocus[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = ComplexZipper[N, A] => Option[ComplexZipper[N, A]]

      def caseZero : Out[_0] = {
        case (tl >>> ((fcs, _))) => Some(tl >>> ((fcs, Nil)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = cz => {

        type SrcM[R] = SourceM[P, A, R]
        type SrcS[S, R] = StateT[Option, S, R]

        val MS = MonadState[SrcS, Complex[P, A]]
        import MS._

        for {
          fnst <- focusSpine(cz)
          newTail <- restrictFocus(cz.tail)
          newComplex <- exciseLocal(rootAddr(fnst.dim), fnst).exec(seal(newTail))
        } yield (fromComplex(newComplex) >>> (cz.head._1, Nil))

      }

    })(cz.length.pred)(cz)

  //============================================================================================
  // CONTRACT FOCUS
  //

  def contractFocus[N <: Nat, A](cz : ComplexZipper[N, A]) : Option[ComplexZipper[N, A]] = 
    (new NatCaseSplit {

      type Out[N <: Nat] = ComplexZipper[N, A] => Option[ComplexZipper[N, A]]

      def caseZero : Out[_0] = {
        case (tl >>> ((fcs, cntxt))) => Some(tl >>> ((Obj(baseValue(fcs)), cntxt)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = cz => {
        for {
          spine <- focusSpine(cz)
          newTail <- compressFocus(cz.tail, spine)
          nstZp = cz.head
        } yield newTail >>> ((Dot(baseValue(nstZp._1), cz.length.pred), nstZp._2))
      }

    })(cz.length.pred)(cz)

  //============================================================================================
  // COMPRESS FOCUS
  //

  def compressFocus[N <: Nat, A](cz : ComplexZipper[N, A], tr : Tree[S[N], A]) : Option[ComplexZipper[N, A]] = 
    for {
      nst <- compressLocal(cz, tr)
    } yield updateFocus(cz, Box(focusValue(cz), nst))

  def compressLocal[N <: Nat, A](cz : ComplexZipper[N, A], tr : Tree[S[N], A]) : Option[Tree[N, Nesting[N, A]]] = 
    tr match {
      case Leaf(_) => focusUnit(cz)
      case Node(_, sh) => {

        import scalaz.std.option._

        for {
          cp <- focusCanopy(cz)
          zsh <- Tree.matchTree(cp, sh)
          toJoin <- traverse(zsh)({ case (d, t) => 
            for {
              cz0 <- visitComplex(d, cz)
              lr <- compressLocal(cz0, t)
            } yield lr
          })
          result <- Tree.join(toJoin)
        } yield result
      }

    }

}
