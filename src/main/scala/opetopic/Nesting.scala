/**
  * Nesting.scala - Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scala.language.higherKinds
import scala.language.implicitConversions

import scalaz.Id._
import scalaz.Monad
import scalaz.Applicative
import scalaz.syntax.monad._

import TypeDefs._

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

  def map[A, B, N <: Nat](nst: Nesting[A, N])(f: A => B) : Nesting[B, N] = 
    traverse[Id, A, B, N](nst)(f)

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

  def traverseWithAddress[T[_], A, B, N <: Nat](nst : Nesting[A, N])(
    f : (A, Address[S[N]]) => T[B]
  )(implicit apT : Applicative[T]) : T[Nesting[B, N]] =
    traverseWithAddress(nst, Nil)(f)

  //============================================================================================
  // MATCH TRAVERSE
  //

  def matchTraverse[A, B, C, N <: Nat](
    nstA : Nesting[A, N], nstB : Nesting[B, N]
  )(f : (A, B) => ShapeM[C]) : ShapeM[Nesting[C, N]] = {

    val apM = Applicative[ShapeM]
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
      case _ => fail(new ShapeError("Match failed"))
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

  def spineFromCanopy[A, N <: Nat](cn : Tree[Nesting[A, N], N]) : ShapeM[Tree[A, N]] = 
    for {
      toJoin <- Tree.traverseWithLocalData(cn)({
        case (nst, _, deriv) => spineFromDerivative(nst, deriv)
      })
      result <- Tree.join(toJoin)
    } yield result

  //============================================================================================
  // SPINE FROM DERIVATIVE
  //

  def spineFromDerivative[A, N <: Nat](nst : Nesting[A, N], deriv : Derivative[A, N]) : ShapeM[Tree[A, N]] = 
    nst match {
      case Obj(a) => Monad[ShapeM].pure(Pt(a))
      case Dot(a, d) => Monad[ShapeM].pure(Zipper.plug(d)(deriv, a))
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

  def visitNesting[A, N <: Nat](n : N)(zipper: NestingZipper[A, N], dir: Address[N]) : ShapeM[NestingZipper[A, N]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (Address[N], NestingZipper[A, N]) => ShapeM[NestingZipper[A, N]]

      def caseZero : Out[_0] = {
        case (d, (Obj(_), cntxt)) => fail(new ShapeError("Cannot visit and object"))
        case (d, (Box(a, Pt(int)), cntxt)) => 
          Monad[ShapeM].pure(int, (a, ()) :: cntxt)
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (d, (Dot(_, _), cntxt)) => fail(new ShapeError("Cannot visit a dot"))
        case (d, (Box(a, canopy), cntxt)) => 
          for {
            loc <- Tree.seekTo(canopy, d)
            res <- (
              loc._1 match {
                case Leaf(_) => fail(new ShapeError("Ran out of room in canopy"))
                case Node(nst, hsh) => 
                  Monad[ShapeM].pure((nst, (a, (hsh, loc._2)) :: cntxt))
              }
            )
          } yield res
      }

    })(n)(dir, zipper)

  def seekNesting[A, N <: Nat](n: N)(z: NestingZipper[A, N], addr: Address[S[N]]) : ShapeM[NestingZipper[A, N]] = 
    addr match {
      case Nil => Monad[ShapeM].pure(z)
      case (d :: ds) =>
        for {
          zp <- seekNesting(n)(z, ds)
          zr <- visitNesting(n)(zp, d)
        } yield zr
    }

  def sibling[A, N <: Nat](n : N)(z: NestingZipper[A, S[N]], addr: Address[N]) : ShapeM[NestingZipper[A, S[N]]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = (NestingZipper[A, S[N]], Address[N]) => ShapeM[NestingZipper[A, S[N]]]

      def caseZero : Out[_0] = {
        case ((nst, Nil), addr) => fail(new ShapeError("First Sibling error"))
        case ((nst, (a, (Pt(Leaf(d)), hcn)) :: cntxt), addr) => fail(new ShapeError("Second Sibling error"))
        case ((nst, (a, (Pt(Node(nfcs, sh)), hcn)) :: cntxt), addr) =>
          Monad[ShapeM].pure(nfcs, (a, (sh, (nst, ()) :: hcn)) :: cntxt)
      }

      def caseSucc[P <: Nat](p : P) : (NestingZipperDblSucc[A, P], Address[S[P]]) => ShapeM[NestingZipperDblSucc[A, P]] = {
        case ((nst, Nil), addr) => fail(new ShapeError("Third Sibling Error"))
        case ((nst, (a, (verts, hcn)) :: cntxt), addr) => 
          for {
            vzip <- Tree.seekTo(verts, addr)
            res <- (
              vzip._1 match {
                case Leaf(_) => fail(new ShapeError("Fourth Sibling Error"))
                case Node(Leaf(_), _) => fail(new ShapeError("Fifth Sibling Error"))
                case Node(Node(nfcs, vrem), hmask) => 
                  Monad[ShapeM].pure((nfcs, (a, (vrem, (nst, (hmask, vzip._2)) :: hcn)) :: cntxt))
              }
            )
          } yield res
      }

    })(n)(z, addr)

  def predecessor[A, N <: Nat](nz: NestingZipper[A, N]) : ShapeM[NestingZipper[A, N]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = NestingZipper[A, N] => ShapeM[NestingZipper[A, N]]

      def caseZero : Out[_0] = {
        nz => fail(new ShapeError("No predecessor for object"))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = {
        case (fcs, Nil) => fail(new ShapeError("No predecessor"))
        case (fcs, (a, (verts, Nil)) :: cs) => fail(new ShapeError("No predecessor"))
        case (fcs, (a, (verts, (pred, deriv) :: vs)) :: cs) => {
          Monad[ShapeM].pure(pred, (a, (Zipper.plug(p)(deriv, Node(fcs, verts)), vs)) :: cs)
        }
      }

    })(nz._1.dim)(nz)

  def predecessorWhich[A, N <: Nat](nz: NestingZipper[A, N])(f: A => Boolean) : ShapeM[NestingZipper[A, N]] = 
    (new NatCaseSplit0 {

      type Out[N <: Nat] = NestingZipper[A, N] => ShapeM[NestingZipper[A, N]]

      def caseZero : Out[_0] = { 
        nz => if (f(baseValue(nz._1))) Monad[ShapeM].pure(nz) else 
          fail(new ShapeError("No predecessor"))
      }

      def caseSucc[P <: Nat](p : P) : Out[S[P]] = 
        nz => if (f(baseValue(nz._1))) Monad[ShapeM].pure(nz) else 
          for {
            pred <- predecessor(nz)
            res <- predecessorWhich(pred)(f)
          } yield res

    })(nz._1.dim)(nz)

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

  //============================================================================================
  // FOREACH
  //

  def foreach[A, N <: Nat](nst: Nesting[A, N])(op: A => Unit) : Unit = 
    nst match {
      case Obj(a) => op(a)
      case Dot(a, _) => op(a)
      case Box(a, cn) => {
        Tree.foreach(cn)(foreach(_)(op))
        op(a)
      }
    }

  //============================================================================================
  // EXTEND NESTING
  //

  def extendNesting[A, B, N <: Nat](nst : Nesting[A, N], addr : Address[S[N]])(f : Address[S[N]] => B) : Tree[Nesting[B, S[N]], S[N]] = 
    nst match {
      case Obj(a) => Leaf(__1)
      case Dot(a, d) => Leaf(S(d))
      case Box(a, cn) => {
        Node(Dot(f(addr), S(cn.dim)), Tree.mapWithAddress(cn)({
          case (n, dir) => extendNesting(n, dir :: addr)(f)
        }))
      }
    }

  def extendNesting[A, B, N <: Nat](nst : Nesting[A, N])(f : Address[S[N]] => B) : Tree[Nesting[B, S[N]], S[N]] = 
    extendNesting(nst, Nil)(f)

}

object Nesting extends NestingFunctions

