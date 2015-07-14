/**
  * Complex.scala - Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scalaz.Monad
import scalaz.StateT
import scalaz.MonadState
import scalaz.Applicative

import scalaz.syntax.monad._

trait ComplexFunctions {

  def complexToZipper[A[_ <: Nat], N <: Nat](c: Complex[A, N]) : ComplexZipper[A, N] = {

    type IdxdNst[K <: Nat] = Nesting[A[K], K]
    type IdxdZip[K <: Nat] = NestingZipper[A[K], K]

    Suite.map[IdxdNst, IdxdZip, S[N]](c)(new IndexedMap[IdxdNst, IdxdZip] {
      def apply[K <: Nat](k: K)(nst : IdxdNst[K]) : IdxdZip[K] = (nst, Nil)
    })

  }

  def seal[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N]) : Complex[A, N] = {

    type IdxdNst[K <: Nat] = Nesting[A[K], K]
    type IdxdZip[K <: Nat] = NestingZipper[A[K], K]

    Suite.map[IdxdZip, IdxdNst, S[N]](z)(new IndexedMap[IdxdZip, IdxdNst] {
      def apply[K <: Nat](k: K)(zp : IdxdZip[K]) : IdxdNst[K] = Nesting.closeNesting(zp._1.dim)(zp._2, zp._1)
    })

  }

  def complexHead[A[_ <: Nat], N <: Nat](z: Complex[A, N]) : Nesting[A[N], N] = 
    Suite.head[Lambda[`K <: Nat` => Nesting[A[K], K]], N](z)

  def complexTail[A[_ <: Nat], N <: Nat](z: Complex[A, S[N]]) : Complex[A, N] = 
    Suite.tail[Lambda[`K <: Nat` => Nesting[A[K], K]], S[N]](z)

  def zipperHead[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N]) : NestingZipper[A[N], N] = 
    Suite.head[Lambda[`K <: Nat` => NestingZipper[A[K], K]], N](z)

  def zipperTail[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, S[N]]) : ComplexZipper[A, N] = 
    Suite.tail[Lambda[`K <: Nat` => NestingZipper[A[K], K]], S[N]](z)

  def focusOf[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N]) : Nesting[A[N], N] = 
    z match { case ComplexZipper(_, (fcs, _)) => fcs }

  def contextOf[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N]) : NestingContext[A[N], N] = 
    z match { case ComplexZipper(_, (_, cntxt)) => cntxt }

  def updateFocus[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N], nst: Nesting[A[N], N]) : ComplexZipper[A, N] = 
    z match { case ComplexZipper(tl, (_, cntxt)) => tl >> (nst, cntxt) }

  def focusValue[A[_ <: Nat], N <: Nat](z: ComplexZipper[A, N]) : A[N] =
    Nesting.baseValue(focusOf(z))

  @natElim
  def focusDeriv[A[_ <: Nat], N <: Nat](n: N)(z: ComplexZipper[A, N]) : ShapeM[Derivative[A[S[N]], S[N]]] = {
    case (Z, ComplexZipper(_, (Obj(a), _))) => succeed(Pt(Leaf(__1)), Nil)
    case (Z, ComplexZipper(_, (Box(a, cn), _))) => succeed(Tree.const(cn, Leaf(__1)), Nil)
    case (S(p), ComplexZipper(_, (Dot(a, d), _))) => fail("No derivative at a dot")
    case (S(p), ComplexZipper(_, (Box(a, cn), _))) => succeed(Tree.const(cn, Leaf(S(S(p)))), Nil)
  }

  @natElim
  def focusSpine[A[_ <: Nat], N <: Nat](n: N)(z: ComplexZipper[A, N]) : ShapeM[Tree[A[N], N]] = {
    case (Z, ComplexZipper(_, (Obj(a), _))) => succeed(Pt(a))
    case (Z, ComplexZipper(_, (Box(a, cn), _))) => Nesting.spineFromCanopy(cn)
    case (S(p), ComplexZipper(z, (Dot(a, d), cntxt))) => for { deriv <- focusDeriv(p)(z) } yield { Zipper.plug(S(p))(deriv, a) }
    case (S(p), ComplexZipper(z, (Box(a, cn), cntxt))) => Nesting.spineFromCanopy(cn)

  }

  @natElim
  def focusCanopy[A[_ <: Nat], N <: Nat](n: N)(z: ComplexZipper[A, N]) : ShapeM[Tree[Address[N], N]] = {
    case (Z, _) => succeed(Pt(()))
    case (S(p), ComplexZipper(z, (Dot(a, d), cs))) => fail("Dot has no canopy")
    case (S(p), ComplexZipper(z, (Box(a, cn), cs))) => succeed(Tree.mapWithAddress(cn)({ case (_, addr) => addr }))
  }

  @natElim
  def focusUnit[A[_ <: Nat], N <: Nat](n: N)(z: ComplexZipper[A, N]) : ShapeM[Tree[Nesting[A[N], N], N]] = {
    case (Z, z) => succeed(Pt(focusOf(z)))
    case (S(p), z) => 
      for {
        tr <- focusSpine(S(p))(z)
        res <- (
          tr match {
            case Leaf(d) =>
              for {
                u <- focusUnit(p)(zipperTail(z))
              } yield Node(focusOf(z), Tree.const(u, Leaf(d)))
            case Node(a, sh) =>
              for {
                extents <- Tree.shellExtents(sh)
              } yield Node(focusOf(z), Tree.const(extents, Leaf(S(p))))
          }
        )
      } yield res
  }

  @natElim
  def visitComplex[A[_ <: Nat], N <: Nat](n: N)(z: ComplexZipper[A, N], dir: Address[N]) : ShapeM[ComplexZipper[A, N]] = {
    case (Z, ComplexZipper(_, nst), _) =>
      for {
        z0 <- Nesting.visitNesting(__0)(nst, ())
      } yield ComplexZipper[A]() >> z0
    case (S(p), z, Nil) =>
      for {
        z0 <- Nesting.visitNesting(S(p))(zipperHead(z), Nil)
      } yield zipperTail(z) >> z0
    case (S(p), z, d :: ds) =>
      for {
        z0 <- visitComplex(S(p))(z, ds)
        z1 <- Nesting.sibling(p)(zipperHead(z0), d)
        tr <- focusSpine(S(p))(z0)
        res <- (
          tr match {
            case Leaf(_) =>
              succeed(zipperTail(z0) >> z1)
            case Node(a, sh) =>
              for {
                extents <- Tree.shellExtents(sh)
                recAddr <- Tree.valueAt(extents, d)
                tl <- seekComplex(p)(zipperTail(z0), recAddr)
              } yield (tl >> z1)
          }
        )
      } yield res
  }

  def seekComplex[A[_ <: Nat], N <: Nat](n: N)(z: ComplexZipper[A, N], addr: Address[S[N]]) : ShapeM[ComplexZipper[A, N]] =
    addr match {
      case Nil => succeed(z)
      case (d :: ds) => 
        for { 
          z0 <- seekComplex(n)(z, ds) 
          z1 <- visitComplex(n)(z0, d)
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

  def sourceAt[A[_ <: Nat], N <: Nat](n: N)(c: Complex[A, N], addr: Address[S[N]]): ShapeM[Complex[A, N]] =
    for {
      c0 <- restrictAt(n)(c, addr)
      res <- contractAt(n)(c0, Nil)
    } yield res

  def restrictAt[A[_ <: Nat], N <: Nat](n: N)(c: Complex[A, N], addr: Address[S[N]]) : ShapeM[Complex[A, N]] =
    for {
      z <- seekComplex(n)(complexToZipper(c), addr)
      z0 <- restrictFocus(n)(z)
    } yield seal(z0)

  def contractAt[A[_ <: Nat], N <: Nat](n: N)(c: Complex[A, N], addr: Address[S[N]]) : ShapeM[Complex[A, N]] =
    for {
      z <- seekComplex(n)(complexToZipper(c), addr)
      z0 <- contractFocus(n)(z)
    } yield seal(z0)

  @natElim
  def restrictFocus[A[_ <: Nat], N <: Nat](n: N)(z: ComplexZipper[A, N]) : ShapeM[ComplexZipper[A, N]] = {
    case (Z, z) => succeed(ComplexZipper[A]() >> (focusOf(z), Nil))
    case (S(p), z) => 
      for {
        tr <- focusSpine(S(p))(z)
        tl <- restrictFocus(p)(zipperTail(z))
        c <- exciseLocal(p)(Nil, tr).exec(seal(tl))
      } yield complexToZipper(c) >> (focusOf(z), Nil)
  }

  @natElim
  def contractFocus[A[_ <: Nat], N <: Nat](n: N)(z:  ComplexZipper[A, N]) : ShapeM[ComplexZipper[A, N]] = {
    case (Z, z) => succeed(updateFocus(z, Obj(focusValue(z))))
    case (S(p), z) =>
      for {
        tr <- focusSpine(S(p))(z)
        tl <- compressFocus(p)(zipperTail(z), tr)
      } yield tl >> (Dot(focusValue(z), S(p)), contextOf(z))
  }

  def compressFocus[A[_ <: Nat], N <: Nat](n: N)(z: ComplexZipper[A, N], tr: Tree[A[S[N]], S[N]]) : ShapeM[ComplexZipper[A, N]] =
    for {
      cn <- compressLocal(n)(z, tr)
    } yield updateFocus(z, Box(focusValue(z), cn))

  def compressLocal[A[_ <: Nat], N <: Nat](n: N)(z: ComplexZipper[A, N], tr: Tree[A[S[N]], S[N]]) : ShapeM[Tree[Nesting[A[N], N], N]] =
    tr match {
      case Leaf(_) => focusUnit(n)(z)
      case Node(a, sh) =>
        for {
          cn <- focusCanopy(n)(z)
          toJn <- Tree.matchTraverse(cn, sh)({
            case (d, tr) =>
              for {
                z0 <- visitComplex(n)(z, d)
                r <- compressLocal(n)(z0, tr)
              } yield r
          })
          res <- Tree.join(toJn)
        } yield res
    }

  def exciseLocal[A[_ <: Nat], N <: Nat](n: N)(addr: Address[S[N]], tr: Tree[A[S[N]], S[N]]) : SourceM[A, N, Unit] = {

    type SrcM[R] = SourceM[A, N, R]
    type SrcS[S, R] = StateT[ShapeM, S, R]

    val MS = MonadState[SrcS, Complex[A, N]]
    import MS._

    tr match {
      case Leaf(_) =>
        for {
          complex <- get
          contractResult <- liftS(contractAt(n)(complex, addr))
          _ <- put(contractResult)
        } yield ()
      case Node(a, sh) =>
        for {
          _ <- Tree.traverseWithAddress[SrcM, Tree[A[S[N]], S[N]], Unit, N](sh)({
            case (t, d) => exciseLocal(n)(d :: addr, t)
          })(implicitly[Applicative[SrcM]])
        } yield ()
    }

  }

  //============================================================================================
  // COMULTIPLY
  //

  @natElim
  def comultiply[A[_ <: Nat], N <: Nat](n: N)(cmplx: Complex[A, N]) : ShapeM[DblComplex[A, N]] = {
    case (Z, Complex(_, hd)) => {

      val newNesting : Nesting[Complex[A, _0], _0] =
        Nesting.map(hd)({
          case a => Complex() >> Obj(a)
        })

      succeed(Complex[Lambda[`K <: Nat` => Complex[A, K]]]() >> newNesting)

    }
    case (S(p), Complex(tl, hd)) => {
      for {
        newTail <- comultiply(p)(tl)
        newHead <- Nesting.traverseWithAddress(hd)({
          case (_, addr) => sourceAt(S(p))(tl >> hd, addr)
        })
      } yield newTail >> newHead
    }
  }

  //============================================================================================
  // COMPLEX GRAFTING
  //

  def paste[A[_ <: Nat], N <: Nat](n: N)(pd: Tree[Complex[A, S[N]], S[N]])(disc: Discriminator[A]) 
      : ShapeM[(Complex[A, N], Tree[Nesting[A[S[N]], S[N]], S[N]])] = 
    for {
      pr <- pastingDiagramToNesting(n)(pd)
      res <- graftNesting(n)(pr._1)(disc)
    } yield (res, pr._2)

  def pastingDiagramToNesting[A[_ <: Nat], N <: Nat](n: N)(pd: Tree[Complex[A, S[N]], S[N]]) 
      : ShapeM[(Nesting[Complex[A, N], N], Tree[Nesting[A[S[N]], S[N]], S[N]])] = {

    val (cmplxTree, pdTree) = Tree.splitWith(pd)(c => (complexTail(c), complexHead(c)))

    for {
      graftNst <- Nesting.toNesting(cmplxTree)({
        case Nil => fail("This should have been a leaf!")
        case (dir :: addr) => 
          for {
            zp <- Tree.seekTo(S(n))(cmplxTree, addr)
            res <- (
              zp._1 match {
                case Leaf(_) => fail("I wanted a complex!")
                case Node(c, _) => sourceAt(n)(c, dir :: Nil)
              }
            )
          } yield res
      })
    } yield (graftNst, pdTree)
  }

  @natElim
  def graftNesting[A[_ <: Nat], N <: Nat](n: N)(nst: Nesting[Complex[A, N], N])(disc: Discriminator[A]) : ShapeM[Complex[A, N]] = {
    case (Z, Obj(c), disc) => succeed(c)
    case (Z, Box(Complex(em, Box(t0, Pt(Obj(s0)))), Pt(pd)), disc) => {
      for {
        resultPd <- graftNesting(Z)(pd)(disc)
        outC = complexHead(resultPd)
        v0 <- disc(Z)(s0, Nesting.baseValue(outC))
      } yield em >> Box(t0, Pt(Nesting.withBase(v0, outC)))
    }
    case (Z, _, _) => fail("Malformed arrow complex")
    case (S(p: P), Dot(c, _), disc) => succeed(c)
    case (S(p: P), Box(Complex(c, Box(t0, cn0)), cn), disc) => {
      for {
        cmplxSh <- Tree.traverse(cn)(graftNesting(S(p))(_)(disc))
        prTr <- Tree.matchWithAddress(S(p))(cn0, cmplxSh)({
          case (Dot(s0, _), Complex(c0, nst), addr) => {
            for {
              v0 <- disc(S(p))(s0, Nesting.baseValue(nst))
            } yield (Nesting.withBase(v0, nst), complexHead(c0))
          }
          case _ => fail("Malformed complex")
        })
        (newCnpy, fillerTr) = Tree.unzip(prTr)
        nstNst <- Nesting.toNesting(fillerTr)(addr => {
          for {
            zp <- Nesting.seekNesting(p)((complexHead(c), Nil), addr)
          } yield zp._1
        })
        cdim2 <- Nesting.nestingJoin(p)(nstNst)
      } yield {
        type INst[K <: Nat] = Nesting[A[K], K]
        (Suite.tail[INst, P](c) >> cdim2 >> Box(t0, newCnpy))
      }
    }
    case (S(p: P), _, _) => fail("Malformed complex")
  }

}

object Complex extends ComplexFunctions {

  def apply[A[_ <: Nat]]() : Suite[({ type L[K <: Nat] = Nesting[A[K], K] })#L, _0] = 
    SNil[({ type L[K <: Nat] = Nesting[A[K], K] })#L]()

  def unapply[A[_ <: Nat], N <: Nat](suite : Suite[({ type L[K <: Nat] = Nesting[A[K], K] })#L, S[N]])
      : Option[(Suite[({ type L[K <: Nat] = Nesting[A[K], K] })#L, N], Nesting[A[N], N])] = {
    type IdxdNesting[K <: Nat] = Nesting[A[K], K]
    Some((Suite.tail[IdxdNesting, N](suite), Suite.head[IdxdNesting, N](suite)))
  }

  import upickle._

  implicit def complexWriter[A[_ <: Nat], N <: Nat](implicit wrtr : IndexedWriter[A]) : Writer[Complex[A, N]] = {
    type IdxdNesting[K <: Nat] = Nesting[A[K], K]
    Suite.suiteWriter(new IndexedWriter[IdxdNesting] {
      def writer[N <: Nat] : Writer[IdxdNesting[N]] = 
        Nesting.nestingWriter[A[N], N](wrtr.writer[N])
    })
  }

  implicit def complexReader[A[_ <: Nat]](implicit rdr: IndexedReader[A]) : Reader[FiniteComplex[A]] = {
    type IdxdNesting[K <: Nat] = Nesting[A[K], K]

    new Reader[FiniteComplex[A]] {
      def read0: PartialFunction[Js.Value, FiniteComplex[A]] = {
        case Js.Arr(els @ _*) => {
          val dim = intToNat(els.length - 1)
          Sigma[({ type L[K <: Nat] = Complex[A, K] })#L, Nat](dim)(readComplex(dim)(els))
        }
      }

      @natElim
      def readComplex[N <: Nat](n: N)(vs: Seq[Js.Value]) : Complex[A, N] = {
        case (Z, vs) => Complex[A]() >> Nesting.nestingReader(Z)(rdr.reader[_0]).read(vs.head)
        case (S(p: P), vs) => readComplex(p)(vs.tail) >> Nesting.nestingReader(S(p))(rdr.reader[S[P]]).read(vs.head)
      }
    }
  }

}
