/**
  * Complex.scala - Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Monad
import scalaz.StateT
import scalaz.MonadState
import scalaz.Applicative

import scalaz.syntax.monad._

import TypeDefs._

trait ComplexFunctions {

  def complexToZipper[A[_ <: Nat], N <: Nat](c: Complex[A, N]) : ComplexZipper[A, N] = {

    type IdxdNst[K <: Nat] = Nesting[A[K], K]
    type IdxdZip[K <: Nat] = NestingZipper[A[K], K]

    Suite.map[IdxdNst, IdxdZip, S[N]](c)(new ~~>[IdxdNst, IdxdZip] {
      def apply[K <: Nat](nst : IdxdNst[K]) : IdxdZip[K] = (nst, Nil)
    })

  }

  def seal[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N]) : Complex[A, N] = {

    type IdxdNst[K <: Nat] = Nesting[A[K], K]
    type IdxdZip[K <: Nat] = NestingZipper[A[K], K]

    Suite.map[IdxdZip, IdxdNst, S[N]](z)(new ~~>[IdxdZip, IdxdNst] {
      def apply[K <: Nat](zp : IdxdZip[K]) : IdxdNst[K] = Nesting.closeNesting(zp._1.dim)(zp._2, zp._1)
    })

  }

  def focusOf[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N]) : Nesting[A[N], N] = 
    z match {
      case ComplexZipper(_, (fcs, _)) => fcs
    }

  def contextOf[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N]) : NestingContext[A[N], N] = 
    z match {
      case ComplexZipper(_, (_, cntxt)) => cntxt
    }

  def updateFocus[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], nst: Nesting[A[N], N]) : ComplexZipper[A, N] = 
    z match {
      case ComplexZipper(tl, (_, cntxt)) => tl >> (nst, cntxt)
    }

  def focusValue[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N]) : A[N] =
    Nesting.baseValue(focusOf(z))

  def focusDeriv[A[_ <: Nat], N <: Nat](z : ComplexZipper[A, N]) : ShapeM[Derivative[A[S[N]], S[N]]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = ComplexZipper[A, N] => ShapeM[Derivative[A[S[N]], S[N]]]

      def caseZero : Out[_0] = {
        case ComplexZipper(_, (Obj(a), _)) => Monad[ShapeM].pure(Pt(Leaf(__1)), Nil)
        case ComplexZipper(_, (Box(a, cn), _)) => Monad[ShapeM].pure(Tree.const(cn, Leaf(__1)), Nil)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case ComplexZipper(z, (Dot(a, d), cntxt)) => fail(new ShapeError)
        case ComplexZipper(z, (Box(a, cn), cntxt)) => Monad[ShapeM].pure(Tree.const(cn, Leaf(S(S(p)))), Nil)
      }

    })(z.length.pred)(z)

  def focusSpine[A[_ <: Nat], N <: Nat](z : ComplexZipper[A, N]) : ShapeM[Tree[A[N], N]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = ComplexZipper[A, N] => ShapeM[Tree[A[N], N]]

      def caseZero : Out[_0] = {
        case ComplexZipper(_, (Obj(a), _)) => Monad[ShapeM].pure(Pt(a))
        case ComplexZipper(_, (Box(a, cn), _)) => Nesting.spineFromCanopy(cn)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case ComplexZipper(z, (Dot(a, d), cntxt)) => for { deriv <- focusDeriv(z) } yield { Zipper.plug(S(p))(deriv, a) }
        case ComplexZipper(z, (Box(a, cn), cntxt)) => Nesting.spineFromCanopy(cn)
      }

    })(z.length.pred)(z)

  def focusCanopy[A[_ <: Nat], N <: Nat](z : ComplexZipper[A, N]) : ShapeM[Tree[Address[N], N]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = ComplexZipper[A, N] => ShapeM[Tree[Address[N], N]]

      def caseZero : Out[_0] = {
        _ => Monad[ShapeM].pure(Pt(()))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case ComplexZipper(z, (Dot(a, d), cntxt)) => 
          fail(new ShapeError)
        case ComplexZipper(z, (Box(a, cn), cntxt)) => 
          Monad[ShapeM].pure(Tree.mapWithAddress(cn)({ case (_, addr) => addr }))
      }

    })(z.length.pred)(z)

  def focusUnit[A[_ <: Nat], N <: Nat](z : ComplexZipper[A, N]) : ShapeM[Tree[Nesting[A[N], N], N]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = ComplexZipper[A, N] => ShapeM[Tree[Nesting[A[N], N], N]]
      type IdxdZip[K <: Nat] = NestingZipper[A[K], K]

      def caseZero : Out[_0] = { z => 
        Monad[ShapeM].pure(Pt(focusOf(z)))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = { z => 
        for {
          tr <- focusSpine(z)
          res <- (
            tr match {
              case Leaf(d) => 
                for { 
                  u <- focusUnit(Suite.tail[IdxdZip, S[P]](z)) 
                } yield Node(focusOf(z), Tree.const(u, Leaf(d)))
              case Node(a, sh) => 
                for {
                  extents <- Tree.shellExtents(sh)
                } yield Node(focusOf(z), Tree.const(extents, Leaf(S(p))))
            }
          )
        } yield res
      }

    })(z.length.pred)(z)

  def visitComplex[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], dir: Address[N]) : ShapeM[ComplexZipper[A, N]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (ComplexZipper[A, N], Address[N]) => ShapeM[ComplexZipper[A, N]]
      type IdxdZip[K <: Nat] = NestingZipper[A[K], K]

      def caseZero : Out[_0] = {
        case (ComplexZipper(_, nst), _) => 
          for {
            z0 <- Nesting.visitNesting(__0)(nst, ())
          } yield ComplexZipper[A]() >> z0
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (c, Nil) => 
          for {
            z0 <- Nesting.visitNesting(S(p))(Suite.head[IdxdZip, S[P]](c), Nil)
          } yield Suite.tail[IdxdZip, S[P]](c) >> z0
        case (c, d :: ds) => 
          for {
            z0 <- visitComplex[A, S[P]](c, ds)
            z1 <- Nesting.sibling(p)(Suite.head[IdxdZip, S[P]](z0), d)
            tr <- focusSpine(z0)
            res <- (
              tr match {
                case Leaf(_) => 
                  Monad[ShapeM].pure(Suite.tail[IdxdZip, S[P]](z0) >> z1)
                case Node(a, sh) => 
                  for {
                    extents <- Tree.shellExtents(sh)
                    recAddr <- Tree.valueAt(extents, d)
                    tl <- seekComplex(Suite.tail[IdxdZip, S[P]](z0), recAddr)
                  } yield (tl >> z1)
              }
            )
          } yield res
      }

    })(z.length.pred)(z, dir)
 
  def seekComplex[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], addr: Address[S[N]]) : ShapeM[ComplexZipper[A, N]] =
    addr match {
      case Nil => Monad[ShapeM].pure(z)
      case (d :: ds) => 
        for { 
          z0 <- seekComplex(z, ds) 
          z1 <- visitComplex(z0, d)
        } yield z1
    } 

  type SourceM[A[_ <: Nat], N <: Nat, R] = StateT[ShapeM, Complex[A, N], R]

  // Fuck, man, figure out how to do this correctly with implicits ...
  def liftS[A[_ <: Nat], N <: Nat, R](mr : ShapeM[R]) : SourceM[A, N, R] = 
    StateT((cmplx : Complex[A, N]) => {
      import scalaz.-\/
      import scalaz.\/-

      mr match {
        case -\/(se) => -\/(se)
        case \/-(r) => \/-(cmplx, r)
      }
    })

  def sourceAt[A[_ <: Nat], N <: Nat](c: Complex[A, N], addr: Address[S[N]]): ShapeM[Complex[A, N]] =
    for {
      c0 <- restrictAt(c, addr)
      res <- contractAt(c0, Nil)
    } yield res

  def restrictAt[A[_ <: Nat], N <: Nat](c: Complex[A, N], addr: Address[S[N]]) : ShapeM[Complex[A, N]] =
    for {
      z <- seekComplex(complexToZipper(c), addr)
      z0 <- restrictFocus(z)
    } yield seal(z0)

  def contractAt[A[_ <: Nat], N <: Nat](c: Complex[A, N], addr: Address[S[N]]) : ShapeM[Complex[A, N]] =
    for {
      z <- seekComplex(complexToZipper(c), addr)
      z0 <- contractFocus(z)
    } yield seal(z0)

  def restrictFocus[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N]) : ShapeM[ComplexZipper[A, N]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = ComplexZipper[A, N] => ShapeM[ComplexZipper[A, N]]
      type IdxdZip[K <: Nat] = NestingZipper[A[K], K]

      def caseZero : Out[_0] = { z =>
        Monad[ShapeM].pure(ComplexZipper[A]() >> (focusOf(z), Nil))
      }

      def caseSucc[P <: Nat](p: P) : Out[S[P]] = { z =>
        for {
          tr <- focusSpine(z)
          tl <- restrictFocus(Suite.tail[IdxdZip, S[P]](z))
          c <- exciseLocal(Nil, tr).exec(seal(tl))
        } yield complexToZipper(c) >> (focusOf(z), Nil)

      }

    })(z.length.pred)(z)

  def contractFocus[A[_ <: Nat], N <: Nat](z:  ComplexZipper[A, N]) : ShapeM[ComplexZipper[A, N]] =
    (new NatCaseSplit0 {

      type Out[N <: Nat] = ComplexZipper[A, N] => ShapeM[ComplexZipper[A, N]]
      type IdxdZip[K <: Nat] = NestingZipper[A[K], K]

      def caseZero : Out[_0] = { z =>
        Monad[ShapeM].pure(updateFocus(z, Obj(focusValue(z))))
      }

      def caseSucc[P <: Nat](p: P) : Out[S[P]] = { z =>
        for {
          tr <- focusSpine(z)
          tl <- compressFocus(Suite.tail[IdxdZip, S[P]](z), tr)
        } yield tl >> (Dot(focusValue(z), S(p)), contextOf(z))
      }

    })(z.length.pred)(z)

  def compressFocus[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], tr: Tree[A[S[N]], S[N]]) : ShapeM[ComplexZipper[A, N]] =
    for {
      cn <- compressLocal(z, tr)
    } yield updateFocus(z, Box(focusValue(z), cn))

  def compressLocal[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], tr: Tree[A[S[N]], S[N]]) : ShapeM[Tree[Nesting[A[N], N], N]] =
    tr match {
      case Leaf(_) => focusUnit(z)
      case Node(a, sh) =>
        for {
          cn <- focusCanopy(z)
          toJn <- Tree.matchTraverse(cn, sh)({
            case (d, tr) =>
              for {
                z0 <- visitComplex(z, d)
                r <- compressLocal(z0, tr)
              } yield r
          })
          res <- Tree.join(toJn)
        } yield res
    }

  def exciseLocal[A[_ <: Nat], N <: Nat](addr: Address[S[N]], tr: Tree[A[S[N]], S[N]]) : SourceM[A, N, Unit] = {

    type SrcM[R] = SourceM[A, N, R]
    type SrcS[S, R] = StateT[ShapeM, S, R]

    val MS = MonadState[SrcS, Complex[A, N]]
    import MS._

    tr match {
      case Leaf(_) =>
        for {
          complex <- get
          contractResult <- liftS(contractAt(complex, addr))
          _ <- put(contractResult)
        } yield ()
      case Node(a, sh) =>
        for {
          _ <- Tree.traverseWithAddress[SrcM, Tree[A[S[N]], S[N]], Unit, N](sh)({
            case (t, d) => exciseLocal(d :: addr, t)
          })(implicitly[Applicative[SrcM]])
        } yield ()
    }

  }

  //============================================================================================
  // COMULTIPLY
  //

  def comultiply[A[_ <: Nat], N <: Nat](cmplx: Complex[A, N]) : ShapeM[DblComplex[A, N]] = 
    (new NatCaseSplit0 {

      type AComplex[K <: Nat] = Complex[A, K]

      type Out[N <: Nat] = Complex[A, N] => ShapeM[DblComplex[A, N]]

      def caseZero : Out[_0] = {
        case cm @ Complex(_, hd) => {

          val newNesting : Nesting[Complex[A, _0], _0] = 
            Nesting.map(hd)({ 
              case a => Complex() >> Obj(a)
            })

          Monad[ShapeM].pure(Complex[AComplex]() >> newNesting)

        }
      }

      def caseSucc[P <: Nat](p: P) : Out[S[P]] = {
        case cm @ Complex(tl, hd) => {
          for {
            newTail <- this(p)(tl)
            newHead <- Nesting.traverseWithAddress(hd)({ 
              case (_, addr) => sourceAt(cm, addr)
            })
          } yield newTail >> newHead
        }
      }

    })(cmplx.length.pred)(cmplx)

}

object Complex extends ComplexFunctions {

  def apply[A[_ <: Nat]]() : Suite[({ type L[K <: Nat] = Nesting[A[K], K] })#L, _0] = 
    SNil[({ type L[K <: Nat] = Nesting[A[K], K] })#L]()

  def unapply[A[_ <: Nat], N <: Nat](suite : Suite[({ type L[K <: Nat] = Nesting[A[K], K] })#L, S[N]])
      : Option[(Suite[({ type L[K <: Nat] = Nesting[A[K], K] })#L, N], Nesting[A[N], N])] = {
    type IdxdNesting[K <: Nat] = Nesting[A[K], K]
    Some((Suite.tail[IdxdNesting, N](suite), Suite.head[IdxdNesting, N](suite)))
  }

}
