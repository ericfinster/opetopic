/**
  * Nesting.scala - Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Applicative
import scalaz.syntax.monad._

sealed abstract class Nesting[+A, N <: Nat] { def dim : N }
case class Obj[+A](a : A) extends Nesting[A, _0] { def dim = Z }
case class Dot[+A, P <: Nat](a : A, d : S[P]) extends Nesting[A, S[P]] { def dim = d }
case class Box[+A, N <: Nat](a : A, c : Tree[Nesting[A, N], N]) extends Nesting[A, N] { def dim = c.dim }

trait NestingFunctions {

  //============================================================================================
  // TRAVERSE
  //

  def traverse[T[_], A, B, N <: Nat](nst : Nesting[A, N])(f : A => T[B])(implicit apT : Applicative[T]) : T[Nesting[B, N]] = {

    import apT.{pure, ap, ap2}

    nst match {
      case Obj(a) => ap(f(a))(pure(Obj(_)))
      case Dot(a, d) => ap2(f(a), pure(d))(pure(Dot(_, _)))
      case Box(a, c) => ap2(f(a), Tree.traverse(c)(traverse(_)(f)))(pure(Box(_, _)))
    }

  }

  //============================================================================================
  // TRAVERSE WITH ADDRESS
  //

  def traverseWithAddress[T[_], A, B, N <: Nat](
    nst : Nesting[A, N], base : Address[S[N]]
  )(f : (A, Address[S[N]]) => T[B])(implicit apT : Applicative[T]) : T[Nesting[B, N]] = {

    import apT.{pure, ap, ap2}

    def traverseCanopy(cn : Tree[Nesting[A, N], N], base : Address[S[N]]) : T[Tree[Nesting[B, N], N]] = 
      Tree.traverseWithAddress(cn)({
        (n, dir) => traverseWithAddress[T, A, B, N](n, dir :: base)(f)
      })

    nst match {
      case Obj(a) => ap(f(a, base))(pure(Obj(_)))
      case Dot(a, d) => ap2(f(a, base), pure(d))(pure(Dot(_, _)))
      case Box(a, c) => ap2(f(a, base), traverseCanopy(c, base))(pure(Box(_, _)))
    }

  }

  //============================================================================================
  // MATCH TRAVERSE
  //

  def matchTraverse[M[+_], A, B, C, N <: Nat](
    nstA : Nesting[A, N], nstB : Nesting[B, N]
  )(f : (A, B) => M[C])(implicit sm : ShapeMonad[M]) : M[Nesting[C, N]] = {

    import sm.failWith

    val apM = Applicative[M]
    import apM.{pure, ap, ap2}

    (nstA, nstB) match {
      case (Obj(a), Obj(b)) => ap(f(a, b))(pure(Obj(_)))
      case (Dot(a, d), Dot(b, _)) => ap2(f(a, b), pure(d))(pure(Dot(_, _)))
      case (Box(a, cnA), Box(b, cnB)) => {

        val cn = Tree.matchTraverse(cnA, cnB)({
          case (nA, nB) => matchTraverse(nA, nB)(f)
        })

        ap2(f(a, b), cn)(pure(Box(_, _)))
      }
      case _ => failWith(new ShapeMatchError())
    }
  }

  //============================================================================================
  // BASE VALUE
  //

  def baseValue[A, N <: Nat](nst: Nesting[A, N]) : A = 
    nst match {
      case Obj(a) => a
      case Dot(a, _) => a
      case Box(a, _) => a
    }

  //============================================================================================
  // TO TREE
  //

  def toTree[A, N <: Nat](nst: Nesting[A, N]) : Tree[A, S[N]] = 
    nst match {
      case Obj(a) => Leaf(__1)
      case Dot(a, d) => Leaf(S(d))
      case Box(a, cn) => Node(a, Tree.map(cn)(toTree(_)))
    }

  //============================================================================================
  // SPINE FROM CANOPY
  //

  def spineFromCanopy[M[+_], A, N <: Nat](cn : Tree[Nesting[A, N], N])(implicit sm : ShapeMonad[M]) : M[Tree[A, N]] = 
    for {
      toJoin <- Tree.traverseWithLocalData(cn)({
        case (nst, _, deriv) => spineFromDerivative(nst, deriv)
      })
      result <- Tree.join(toJoin)
    } yield result

  //============================================================================================
  // SPINE FROM DERIVATIVE
  //

  def spineFromDerivative[M[+_], A, N <: Nat](nst : Nesting[A, N], deriv : Derivative[A, N])(implicit sm: ShapeMonad[M]) : M[Tree[A, N]] = 
    nst match {
      case Obj(a) => sm.pure(Pt(a))
      case Dot(a, d) => sm.pure(Zipper.plug(d)(deriv, a))
      case Box(a, cn) => spineFromCanopy(cn)
    }

  //============================================================================================
  // NESTING ZIPPER OPS
  //

  def plugNesting[A, N <: Nat](n: N)(deriv: NestingDerivative[A, N], a: A) : Nesting[A, N] = 
    closeNesting(n)(deriv._2, Box(a, deriv._1))

  def closeNesting[A, N <: Nat](n: N)(cntxt: NestingContext[A, N], nst: Nesting[A, N]) : Nesting[A, N] = 
    cntxt match {
      case Nil => nst
      case (a, d) :: cs => closeNesting(n)(cs, Box(a, Zipper.plug(n)(d, nst)))
    }

  def visitNesting[M[+_], A, N <: Nat](n : N)(zipper: NestingZipper[A, N], dir: Address[N])(implicit sm : ShapeMonad[M]) : M[NestingZipper[A, N]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (Address[N], NestingZipper[A, N]) => M[NestingZipper[A, N]]

      def caseZero : Out[_0] = {
        case (d, (Obj(_), cntxt)) => sm.failWith(new ShapeLookupError)
        case (d, (Box(a, Pt(int)), cntxt)) => 
          sm.pure(int, (a, ()) :: cntxt)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (d, (Dot(_, _), cntxt)) => sm.failWith(new ShapeLookupError)
        case (d, (Box(a, canopy), cntxt)) => 
          for {
            loc <- Tree.seekTo(canopy, d)
            res <- (
              loc._1 match {
                case Leaf(_) => sm.failWith(new ShapeLookupError)
                case Node(nst, hsh) => 
                  sm.pure((nst, (a, (hsh, loc._2)) :: cntxt))
              }
            )
          } yield res
      }

    })(n)(dir, zipper)

  def seekNesting[M[+_], A, N <: Nat](n: N)(z: NestingZipper[A, N], addr: Address[S[N]])(implicit sm: ShapeMonad[M]) : M[NestingZipper[A, N]] = 
    addr match {
      case Nil => sm.pure(z)
      case (d :: ds) =>
        for {
          zp <- seekNesting(n)(z, ds)
          zr <- visitNesting(n)(zp, d)
        } yield zr
    }

  def sibling[M[+_], A, N <: Nat](n : N)(z: NestingZipper[A, S[N]], addr: Address[N])(implicit sm: ShapeMonad[M]) : M[NestingZipper[A, S[N]]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (NestingZipper[A, S[N]], Address[N]) => M[NestingZipper[A, S[N]]]

      def caseZero : Out[_0] = {
        case ((nst, Nil), addr) => sm.failWith(new ShapeLookupError)
        case ((nst, (a, (Pt(Leaf(d)), hcn)) :: cntxt), addr) => sm.failWith(new ShapeLookupError)
        case ((nst, (a, (Pt(Node(nfcs, sh)), hcn)) :: cntxt), addr) =>
          sm.pure(nfcs, (a, (sh, (nst, ()) :: hcn)) :: cntxt)
      }

      def caseSucc[P <: Nat](p : P) : (NestingZipperDblSucc[A, P], Address[S[P]]) => M[NestingZipperDblSucc[A, P]] = {
        case ((nst, Nil), addr) => sm.failWith(new ShapeLookupError)
        case ((nst, (a, (verts, hcn)) :: cntxt), addr) => 
          for {
            vzip <- Tree.seekTo(verts, addr)
            res <- (
              vzip._1 match {
                case Leaf(_) => sm.failWith(new ShapeLookupError)
                case Node(Leaf(_), _) => sm.failWith(new ShapeLookupError)
                case Node(Node(nfcs, vrem), hmask) => 
                  sm.pure((nfcs, (a, (vrem, (nst, (hmask, vzip._2)) :: hcn)) :: cntxt))
              }
            )
          } yield res
      }

    })(n)(z, addr)

//   def predecessor[N <: Nat, A](nz : NestingZipper[N, A]) : Option[NestingZipper[N, A]] = 
//     (new NatCaseSplit {

//       type Out[N <: Nat] = NestingZipper[N, A] => Option[NestingZipper[N, A]]

//       def caseZero : Out[_0] = 
//         nz => None

//       def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
//         case (fcs, Nil) => None
//         case (fcs, (a, (verts, Nil)) :: cs) => None
//         case (fcs, (a, (verts, (pred, deriv) :: vs)) :: cs) => {
//           Some(pred, (a, (plug(p)(deriv, Node(fcs, verts)), vs)) :: cs)
//         }
//       }

//     })(nz._1.dim)(nz)

//   def predecessorWhich[N <: Nat, A](nz : NestingZipper[N, A])(f : A => Boolean) : Option[NestingZipper[N, A]] = 
//     (new NatCaseSplit {

//       type Out[N <: Nat] = NestingZipper[N, A] => Option[NestingZipper[N, A]]

//       def caseZero : Out[_0] = 
//         nz => if (f(nz._1.baseValue)) Some(nz) else None

//       def caseSucc[P <: Nat](p : P) : Out[S[P]] = 
//         nz => if (f(nz._1.baseValue)) Some(nz) else 
//           for {
//             pred <- predecessor(nz)
//             res <- predecessorWhich(pred)(f)
//           } yield res

//     })(nz._1.dim)(nz)

  //============================================================================================
  // CASE SPLITTING
  //

  trait NestingCaseSplit {

    type Out[A, N <: Nat]

    def caseObj[A](a : A) : Out[A, _0]
    def caseDot[A, P <: Nat](a : A, d : S[P]) : Out[A, S[P]]
    def caseBox[A, N <: Nat](a : A, c : Tree[Nesting[A, N], N]) : Out[A, N]

    def apply[A, N <: Nat](nst : Nesting[A, N]) : Out[A, N] = 
      nst match {
        case Obj(a) => caseObj(a)
        case Dot(a, d) => caseDot(a, d)
        case Box(a, c) => caseBox(a, c)
      }

  }

//   //============================================================================================
//   // FOREACH
//   //

//   def foreach[N <: Nat, A](nst : Nesting[N, A])(op : A => Unit) : Unit = 
//     nst match {
//       case Obj(a) => op(a)
//       case Dot(a, _) => op(a)
//       case Box(a, cn) => {
//         for { n <- cn } { foreach(n)(op) }
//         op(a)
//       }
//     }

//   //============================================================================================
//   // EXTEND NESTING
//   //

//   def extendNesting[N <: Nat, A](nst : Nesting[N, A], addr : Address[S[N]])(f : Address[S[N]] => A) : Tree[S[N], Nesting[S[N], A]] = 
//     (new NestingCaseSplit[A] {

//       type Out[N <: Nat, +U <: Nesting[N, A]] = (Address[S[N]], (Address[S[N]] => A)) => Tree[S[N], Nesting[S[N], A]]

//       def caseObj(a : A) : Out[_0, Obj[A]] = 
//         (addr, f) => Leaf(__1)

//       def caseDot[P <: Nat](a : A, d : S[P]) : Out[S[P], Dot[P, A]] = 
//         (addr, f) => Leaf(S(d))

//       def caseBox[N <: Nat](a : A, c : Tree[N, Nesting[N, A]]) : Out[N, Box[N, A]] = 
//         (addr, f) => Node(Dot(f(addr), S(c.dim)), c.mapWithAddress({
//           (n, dir) => extendNesting(n, dir :: addr)(f)
//         }))

//     })(nst)(addr, f)

//   def newNestingExtend[N <: Nat, A](nst : Nesting[N, A])(f : Address[S[N]] => A) : Tree[S[N], Nesting[S[N], A]] = 
//     extendNesting(nst, rootAddr(S(nst.dim)))(f)

//   //============================================================================================
//   // EXTRUDE NESTING
//   //

//   def extrudeNesting[N <: Nat, A, B](a : A, addr : Address[N], tr : Tree[N, Nesting[N, A]], msk : Tree[N, B]) : Option[Tree[N, Nesting[N, A]]] = 
//     (new NatCaseSplit {

//       type Out[N <: Nat] = (Address[N], Tree[N, Nesting[N, A]], Tree[N, B]) => Option[Tree[N, Nesting[N, A]]]

//       def caseZero : Out[_0] = {
//         case (addr, cnpy, msk) => Some(Pt(Box(a, cnpy)))
//       }

//       def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
//         case (addr, cnpy, msk) => 
//           for {
//             zipper <- cnpy seekTo addr 
//             newFcs <- replace(zipper.focus, msk, (c : Tree[S[P], Nesting[S[P], A]]) => Box(a, c))
//           } yield close(S(p))(zipper.context, newFcs)
//       }

//     })(tr.dim)(addr, tr, msk)

}

// trait NestingImplicits {

//   implicit def nestingIsTraverse[N <: Nat] : Traverse[({ type L[+A] = Nesting[N, A] })#L] = 
//     new Traverse[({ type L[+A] = Nesting[N, A] })#L] {

//       override def map[A, B](na : Nesting[N, A])(f : A => B) : Nesting[N, B] = 
//         Nesting.mapNesting(na)(f)

//       def traverseImpl[G[_], A, B](na : Nesting[N, A])(f : A => G[B])(implicit isA : Applicative[G]) : G[Nesting[N, B]] = 
//         Nesting.traverseNesting(na)(f)

//     }

//   import scalaz.syntax.FunctorOps
//   import scalaz.syntax.functor._

//   implicit def nestingToFunctorOps[N <: Nat, A](nst : Nesting[N, A]) : FunctorOps[({ type L[+X] = Nesting[N, X] })#L, A] = 
//     ToFunctorOps[({ type L[+X] = Nesting[N, X] })#L, A](nst)

//   import scalaz.syntax.TraverseOps
//   import scalaz.syntax.traverse._

//   implicit def nestingToTraverseOps[N <: Nat, A](nst : Nesting[N, A]) : TraverseOps[({ type L[+X] = Nesting[N, X] })#L, A] = 
//     ToTraverseOps[({ type L[+X] = Nesting[N, X] })#L, A](nst)

//   class NestingOps[N <: Nat, A](nst : Nesting[N, A]) {

//     def zipWithAddress : Nesting[N, (A, Address[S[N]])] = 
//       Nesting.mapNestingWithAddress(nst)((addr, a) => (a, addr))

//     def mapWithAddress[B](f : (Address[S[N]], A) => B) : Nesting[N, B] = 
//       Nesting.mapNestingWithAddress(nst)(f)

//     def matchWith[B](nstB : Nesting[N, B]) : Option[Nesting[N, (A, B)]] = 
//       Nesting.zipCompleteNesting(nst, nstB)

//     def toTree : Tree[S[N], A] = 
//       Nesting.toTree(nst)

//     def baseValue : A = 
//       Nesting.baseValue(nst)

//     def foreach(op : A => Unit) : Unit = 
//       Nesting.foreach(nst)(op)

//     def seekTo(addr : Address[S[N]]) = 
//       Nesting.seekNesting(addr, (nst, Nil))

//   }

//   implicit def nestingToNestingOps[N <: Nat, A](nst : Nesting[N, A]) : NestingOps[N, A] = 
//     new NestingOps[N, A](nst)

// }

object Nesting extends NestingFunctions
//     with NestingImplicits
