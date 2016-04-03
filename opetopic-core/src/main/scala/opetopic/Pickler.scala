/**
  * Pickle.scala - Custom Pickler implementations for Opetopic types
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import upickle.Js
import upickle.default._
import upickle.Invalid.Data

import scala.{PartialFunction => PF}

object Pickler {

  //============================================================================================
  // TREE PICKLING
  //

  def treeWriter[A, N <: Nat](implicit wrtr: Writer[A]) : Writer[Tree[A, N]] = 
    new Writer[Tree[A, N]] { 
      def write0: Tree[A, N] => Js.Value = {
        case Pt(a) => Js.Obj(("type", Js.Str("pt")), ("val", wrtr.write(a)))
        case Leaf(d) => Js.Obj(("type", Js.Str("lf")))
        case Node(a, sh) => {
          val shellWriter : Writer[Tree[Tree[A, S[Nat]], Nat]] =
            treeWriter[Tree[A, S[Nat]], Nat](this)
          Js.Obj(("type", Js.Str("nd")), ("val", wrtr.write(a)), ("sh", shellWriter.write(sh)))
        }
      }
    }

  @natElim
  def treeReader[A, N <: Nat](n: N)(implicit rdr: Reader[A]) : Reader[Tree[A, N]] = {
    case Z => {
      new Reader[Tree[A, _0]] {
        def read0: PF[Js.Value, Tree[A, _0]] = {
          case Js.Obj(("type", Js.Str("pt")), ("val", a)) => Pt(rdr.read(a))
        }
      }
    }
    case S(p: P) => {
      new Reader[Tree[A, S[P]]] { thisRdr =>
        def read0: PF[Js.Value, Tree[A, S[P]]] = {
          case Js.Obj(("type", Js.Str("lf"))) => Leaf(S(p))
          case Js.Obj(("type", Js.Str("nd")), ("val", a), ("sh", sh)) => {
            val shellReader : Reader[Tree[Tree[A, S[P]], P]] =
              treeReader[Tree[A, S[P]], P](p)(thisRdr)

            Node(rdr.read(a), shellReader.read(sh))
          }
        }
      }
    }
  }

  //============================================================================================
  // NESTING PICKLING
  //

  def nestingWriter[A, N <: Nat](implicit wrtr: Writer[A]) : Writer[Nesting[A, N]] = 
    new Writer[Nesting[A, N]] {
      def write0: Nesting[A, N] => Js.Value = {
        case Obj(a) => Js.Obj(("type", Js.Str("ob")), ("val", wrtr.write(a)))
        case Dot(a, d) => Js.Obj(("type", Js.Str("dt")), ("val", wrtr.write(a)))
        case Box(a, cn) => {
          val canopyWriter : Writer[Tree[Nesting[A, N], N]] = treeWriter(this)
          Js.Obj(("type", Js.Str("bx")), ("val", wrtr.write(a)), ("cn", canopyWriter.write(cn)))
        }
      }
    }

  @natElim
  def nestingReader[A, N <: Nat](n: N)(implicit rdr: Reader[A]) : Reader[Nesting[A, N]] = {
    case Z => {
        new Reader[Nesting[A, _0]] { thisRdr =>
          def read0: PF[Js.Value, Nesting[A, _0]] = {
            case Js.Obj(("type", Js.Str("ob")), ("val", a)) => Obj(rdr.read(a))
            case Js.Obj(("type", Js.Str("bx")), ("val", a), ("cn", cn)) => {
              val canopyReader : Reader[Tree[Nesting[A, _0], _0]] = treeReader(Z)(thisRdr)
              Box(rdr.read(a), canopyReader.read(cn))
            }
          }
        }
    }
    case S(p: P) => {
      new Reader[Nesting[A, S[P]]] { thisRdr =>
        def read0: PF[Js.Value, Nesting[A, S[P]]] = {
          case Js.Obj(("type", Js.Str("dt")), ("val", a)) => Dot(rdr.read(a), S(p))
          case Js.Obj(("type", Js.Str("bx")), ("val", a), ("cn", cn)) => {
            val canopyReader : Reader[Tree[Nesting[A, S[P]], S[P]]] = treeReader(S(p))(thisRdr)
            Box(rdr.read(a), canopyReader.read(cn))
          }
        }
      }
    }
  }

  //============================================================================================
  // INDEXED READING AND WRITING
  //

  trait IndexedWriter[F[_ <: Nat]] {
    def apply[N <: Nat](n: N) : Writer[F[N]]
  }

  trait IndexedReader[F[_ <: Nat]] {
    def apply[N <: Nat](n: N) : Reader[F[N]]
  }

  implicit def toNestingReader[A[_ <: Nat]](implicit ardr: IndexedReader[A]) 
      : IndexedReader[Lambda[`N <: Nat` => Nesting[A[N], N]]] = 
    new IndexedReader[Lambda[`N <: Nat` => Nesting[A[N], N]]] {
      def apply[N <: Nat](n: N) = nestingReader[A[N], N](n)(ardr(n))
    }

  implicit def toNestingWriter[A[_ <: Nat]](implicit awrtr: IndexedWriter[A])
      : IndexedWriter[Lambda[`N <: Nat` => Nesting[A[N], N]]] =
    new IndexedWriter[Lambda[`N <: Nat` => Nesting[A[N], N]]] {
      def apply[N <: Nat](n: N) = nestingWriter[A[N], N](awrtr(n))
    }

  implicit def toOptionReader[A[_ <: Nat]](implicit r: IndexedReader[A]) 
      : IndexedReader[Lambda[`N <: Nat` => Option[A[N]]]] =
    new IndexedReader[Lambda[`N <: Nat` => Option[A[N]]]] {
      def apply[N <: Nat](n: N) = {
        implicit val rn : Reader[A[N]] = r(n)
        implicitly[Reader[Option[A[N]]]]
      }
    }

  implicit def toOptionWriter[A[_ <: Nat]](implicit w: IndexedWriter[A]) 
      : IndexedWriter[Lambda[`N <: Nat` => Option[A[N]]]] =
    new IndexedWriter[Lambda[`N <: Nat` => Option[A[N]]]] {
      def apply[N <: Nat](n: N) = {
        implicit val wn : Writer[A[N]] = w(n)
        implicitly[Writer[Option[A[N]]]]
      }
    }

  //============================================================================================
  // SUITE PICKLING
  //

  // We should write the length so that we can generate a Nat when reading back
  def suiteWriter[F[_ <: Nat], N <: Nat](implicit iwrtr: IndexedWriter[F]) : Writer[Suite[F, N]] = 
    new Writer[Suite[F, N]] {
      def write0: Suite[F, N] => Js.Value = {
        suite => Js.Obj(
          ("type", Js.Str("suite")),
          ("dim", Js.Num(natToInt(suite.length))),
          ("data", Js.Arr(doWrite(suite): _*))
        )
      }

      def doWrite[K <: Nat](s: Suite[F, K]) : List[Js.Value] = 
        Suite.fold[F, List[Js.Value], K](s)(
          new IndexedFold[F, List[Js.Value]] {
            def caseZero = Nil
            def caseSucc[P <: Nat](p: P)(fp: F[P], l: List[Js.Value]) = 
              iwrtr(p).write(fp) :: l
          }
        )
    }

  def suiteReader[F[_ <: Nat]](implicit irdr: IndexedReader[F]) : Reader[FiniteSuite[F]] = 
    new Reader[FiniteSuite[F]] {
      def read0: PF[Js.Value, FiniteSuite[F]] = {
        case Js.Obj(
          ("type", Js.Str("suite")), 
          ("dim", Js.Num(dim)), 
          ("data", Js.Arr(s @ _*))
        ) => {
          val d = intToNat(dim.toInt)
          Sigma[Lambda[`L <: Nat` => Suite[F, L]], Nat](d)(doRead(d)(s.toList))
        }
      }

      @natElim
      def doRead[K <: Nat](k: K)(l: List[Js.Value]) : Suite[F, K] = {
        case (Z, _) => SNil[F]()
        case (S(p: P), l :: ls) => doRead(p)(ls) >> irdr(p).read(l)
        case (S(p: P), Nil) => throw new Data(Js.Null, "Suite was truncated")
      }

    }

  //============================================================================================
  // COMPLEX PICKLING
  //

  def complexWriter[A[_ <: Nat], N <: Nat](implicit wrtr : IndexedWriter[A]) : Writer[Complex[A, N]] = 
    suiteWriter[Lambda[`K <: Nat` => Nesting[A[K], K]], S[N]]

  def complexReader[A[_ <: Nat]](implicit rdr: IndexedReader[A]) : Reader[FiniteComplex[A]] = {
    new Reader[FiniteComplex[A]] {
      def read0: PF[Js.Value, FiniteComplex[A]] = {
        case (v: Js.Value) => {

          type ANst[N <: Nat] = Nesting[A[N], N]
          val s = suiteReader[ANst].read(v)

          import opetopic.syntax.complex._

          @natElim
          def extractFiniteComplex[N <: Nat](n: N)(s: Suite[ANst, N]) : FiniteComplex[A] = {
            case (Z, _) => throw new Data(Js.Null, "Complex cannot be the empty suite")
            case (S(p: P), s) => s
          }

          extractFiniteComplex(s.n)(s.value)

        }
      }
    }
  }

}
