/**
  * Nesting.scala - Nestings
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

import scalaz.Id._
import scalaz.\/
import scalaz.-\/
import scalaz.\/-
import scalaz.Monad
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

  def map[A, B, N <: Nat](nst: Nesting[A, N])(f: A => B) : Nesting[B, N] = 
    traverse[Id, A, B, N](nst)(f)

  //============================================================================================
  // TRAVERSE WITH ADDRESS
  //

  def traverseWithAddress[T[_], A, B, N <: Nat](
    nst : Nesting[A, N], base : Address[S[N]] = Nil
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

  // This should be generalized to work with *any* error monad
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
      case _ => fail("Match failed")
    }
  }

  //============================================================================================
  // ELIM WITH ADDRESS
  //

  // Uhhh .. clean this up like join below so that we don't duplicate the code?

  @natElim
  def elimWithAddress[A, B, N <: Nat](n: N)(nst: Nesting[A, N], addr: Address[S[N]] = Nil)(
    extRec: (A, Address[S[N]]) => B
  )(
    intRec: (A, Address[S[N]], Tree[B, N]) => B
  ) : B = {
    case (Z, Obj(a), addr, er, ir) => er(a, addr)
    case (Z, Box(a, cn), addr, er, ir) => { 
      ir(a, addr,
        Tree.mapWithAddress(cn)({
          case (ns, d) => elimWithAddress(Z)(ns, d :: addr)(er)(ir)
        })
      )
    }
    case (S(p), Dot(a, _), addr, er, ir) => er(a, addr)
    case (S(p), Box(a, cn), addr, er, ir) => {
      ir(a, addr,
        Tree.mapWithAddress(cn)({
          case (ns, d) => elimWithAddress(S(p))(ns, d :: addr)(er)(ir)
        })
      )
    }
  }

  //============================================================================================
  // EXTERNAL
  //

  @natElim
  def external[A, N <: Nat](n: N)(a: A) : Nesting[A, N] = {
    case (Z, a) => Obj(a)
    case (S(p), a) => Dot(a, S(p))
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

  def withBase[A, N <: Nat](a: A, nst: Nesting[A, N]) : Nesting[A, N] = 
    nst match {
      case Obj(_) => Obj(a)
      case Dot(_, d) => Dot(a, d)
      case Box(_, cn) => Box(a, cn)
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

  def treeToNesting[A, N <: Nat](tr: Tree[A, S[N]], addr: Address[S[N]] = Nil)(f: Address[S[N]] => ShapeM[A]) : ShapeM[Nesting[A, N]] = 
    tr match {
      case Leaf(d) => for { a <- f(addr) } yield external(d.pred)(a)
      case Node(a, sh) => 
        for {
          newSh <- Tree.traverseWithAddress(sh)({
            case (b, d) => treeToNesting(b, d :: addr)(f)
          })
        } yield Box(a, newSh)
    }

  //============================================================================================
  // FROM TREE
  //

  def fromTree[A, N <: Nat](tr : Tree[A, S[N]]) : ShapeM[Nesting[\/[A, Address[N]], N]] = 
    Tree.graftRec[A, Nesting[\/[A, Address[N]], N], N](tr)(
      addr => succeed(external(tr.dim.pred)(\/-(addr))))({
      case (a, cn) => succeed(Box(-\/(a), cn)) 
    })

  //============================================================================================
  // SPINE FROM CANOPY
  //

  def spineFromCanopy[A, N <: Nat](cn : Tree[Nesting[A, N], N]) : ShapeM[Tree[A, N]] = 
    Tree.traverseWithLocalData(cn)({
      case (nst, _, deriv) => spineFromDerivative(nst, deriv)
    }).flatMap(Tree.join(_))

  //============================================================================================
  // SPINE FROM DERIVATIVE
  //

  def spineFromDerivative[A, N <: Nat](nst : Nesting[A, N], deriv : Derivative[A, N]) : ShapeM[Tree[A, N]] = 
    nst match {
      case Obj(a) => succeed(Pt(a))
      case Dot(a, d) => succeed(Zipper.plug(d)(deriv, a))
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

  @natElim
  def visitNesting[A, N <: Nat](n : N)(zipper: NestingZipper[A, N], dir: Address[N]) : ShapeM[NestingZipper[A, N]] = {
    case (Z, (Obj(_), cs), dir) => fail("Cannot visit an object")
    case (Z, (Box(a, Pt(cn)), cs), dir) => succeed(cn, (a, ()) :: cs)
    case (S(p), (Dot(_, _), cs), dir) => fail("Cannot visit a dot")
    case (S(p), (Box(a, cn), cs), dir) => 
      for {
        loc <- Tree.seekTo(S(p))(cn, dir)
        res <- (
          loc._1 match {
            case Leaf(_) => fail("Ran out of room in canopy")
            case Node(nst, hsh) =>
              succeed((nst, (a, (hsh, loc._2)) :: cs))
          }
        )
      } yield res
  }

  def seekNesting[A, N <: Nat](n: N)(z: NestingZipper[A, N], addr: Address[S[N]]) : ShapeM[NestingZipper[A, N]] = 
    addr match {
      case Nil => succeed(z)
      case (d :: ds) =>
        for {
          zp <- seekNesting(n)(z, ds)
          zr <- visitNesting(n)(zp, d)
        } yield zr
    }

  def replaceNesting[A, N <: Nat](n: N)(nst: Nesting[A, N], addr: Address[S[N]], a: A) : ShapeM[Nesting[A, N]] = 
    for {
      z <- seekNesting(n)((nst, Nil), addr)
    } yield {

      val newNst : Nesting[A, N] = 
        z._1 match {
          case Obj(_) => Obj(a)
          case Dot(_, d) => Dot(a, d)
          case Box(_, cn) => Box(a, cn)
        }

      closeNesting(n)(z._2, newNst)

    }

  @natElim
  def sibling[A, N <: Nat](n : N)(z: NestingZipper[A, S[N]], addr: Address[N]) : ShapeM[NestingZipper[A, S[N]]] = {
    case (Z, (nst, Nil), addr) => fail("First Sibling error")
    case (Z, (nst, (a, (Pt(Leaf(d)), hcn)) :: cntxt), addr) => fail("Second Sibling error")
    case (Z, (nst, (a, (Pt(Node(nfcs, sh)), hcn)) :: cntxt), addr) =>
      succeed(nfcs, (a, (sh, (nst, ()) :: hcn)) :: cntxt)
    case (S(p), (nst, Nil), addr) => fail("Third Sibling Error")
    case (S(p), (nst, (a, (verts, hcn)) :: cntxt), addr) =>
      for {
        vzip <- Tree.seekTo(verts, addr)
        res <- (
          vzip._1 match {
            case Leaf(_) => fail("Fourth Sibling Error")
            case Node(Leaf(_), _) => fail("Fifth Sibling Error")
            case Node(Node(nfcs, vrem), hmask) =>
              succeed((nfcs, (a, (vrem, (nst, (hmask, vzip._2)) :: hcn)) :: cntxt))
          }
        )
      } yield res
  }

  @natElim
  def predecessor[A, N <: Nat](n: N)(nz: NestingZipper[A, N]): ShapeM[NestingZipper[A, N]] = {
    case (Z, nz) => fail("Object has no predecessor")
    case (S(p), (fcs, Nil)) => fail("No predecessor")
    case (S(p), (fcs, (a, (verts, Nil)) :: cs)) => fail("No predecessor")
    case (S(p), (fcs, (a, (verts, (pred, deriv) :: vs)) :: cs)) => {
      succeed(pred, (a, (Zipper.plug(p)(deriv, Node(fcs, verts)), vs)) :: cs)
    }
  }

  def predecessor[A, N <: Nat](nz: NestingZipper[A, N]) : ShapeM[NestingZipper[A, N]] = 
    predecessor(nz._1.dim)(nz)

  @natElim
  def predecessorWhich[A, N <: Nat](n: N)(nz: NestingZipper[A, N])(f: A => Boolean) : ShapeM[NestingZipper[A, N]] = {
    case (Z, nz, f) => if (f(baseValue(nz._1))) succeed(nz) else fail("No predecessor")
    case (S(p), nz, f) => if (f(baseValue(nz._1))) succeed(nz) else 
      for {
        pred <- predecessor(S(p))(nz)
        res <- predecessorWhich(S(p))(pred)(f)
      } yield res
  }

  def predecessorWhich[A, N <: Nat](nz: NestingZipper[A, N])(f: A => Boolean) : ShapeM[NestingZipper[A, N]] = 
    predecessorWhich(nz._1.dim)(nz)(f)

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

  def extendNesting[A, B, N <: Nat](nst : Nesting[A, N], addr : Address[S[N]] = Nil)(f : Address[S[N]] => B) : Tree[Nesting[B, S[N]], S[N]] = 
    nst match {
      case Obj(a) => Leaf(__1)
      case Dot(a, d) => Leaf(S(d))
      case Box(a, cn) => {
        Node(Dot(f(addr), S(cn.dim)), Tree.mapWithAddress(cn)({
          case (n, dir) => extendNesting(n, dir :: addr)(f)
        }))
      }
    }


  def canopyAddressExtend[A, N <: Nat](n: N)(tr: Tree[Nesting[A, S[N]], S[N]], addr: Address[S[N]] = Nil) : ShapeM[Nesting[Address[S[N]], N]] = 
    tr match {
      case Leaf(_) => succeed(external(n)(addr))
      case Node(nst, sh) => 
        for {
          spine <- spineFromDerivative[A, S[N]](nst, (Tree.const(sh, Leaf(S(n))), Nil))
          shRes <- Tree.traverseWithAddress(sh)({
            case (b, dir) => canopyAddressExtend(n)(b, (dir :: addr))
          })
          res <- Tree.graftRec(n)(spine)(ad => Tree.valueAt(shRes, ad))({ 
            case (_, cn) => succeed(Box(addr, cn)) 
          })
        } yield res
    }

  @natElim
  def nestingJoin[A, N <: Nat](n: N)(nnst: Nesting[Nesting[A, N], N]) : ShapeM[Nesting[A, N]] = {
    case (Z, Obj(obs)) => succeed(obs)
    case (Z, Box(nst, cn)) => nestingJoinCanopy(Z)(Box(nst, cn))
    case (S(p), Dot(ext, _)) => succeed(ext)
    case (S(p), Box(nst, cn)) => nestingJoinCanopy(S(p))(Box(nst, cn))
  }

  def nestingJoinCanopy[A, N <: Nat](n: N)(box: Box[Nesting[A, N], N]) : ShapeM[Nesting[A, N]] = 
    box match {
      case Box(nst, cn) => 
        for {
          trNst <- Tree.traverse(cn)(nestingJoin(n)(_))
          res <- Tree.graftRec[A, Nesting[A, N], N](n)(toTree(nst))(Tree.valueAt(trNst, _))({
            case (an, cnn) => succeed(Box(an, cnn))
          })
        } yield res
    }

  //============================================================================================
  // SUSPENSION
  //

  def suspendBox[A[_ <: Nat], N <: Nat, K <: Nat](n: N, k: K)(
    box: Box[A[N], N], shift: IndexedShift[A, K]
  ) : Box[A[N#Plus[K]], N#Plus[K]] =
    box match {
      case Box(a, cn) => {

        val newCanopy =
          Tree.suspendTree(n, k)(
            Tree.map(cn)(suspendNesting(n, k)(_, shift))
          )

        Box(shift(a), newCanopy)

      }
    }


  @natElim
  def suspendNesting[A[_ <: Nat], N <: Nat, K <: Nat](n: N, k: K)(nst: Nesting[A[N], N], shift: IndexedShift[A, K]) 
      : Nesting[A[N#Plus[K]], N#Plus[K]] = {
    case (Z, Z, nst, shift) => nst
    case (Z, S(q: Q), Obj(a), shift) => Dot(shift(a), S(q))
    case (Z, S(q: Q), Box(a, cn), shift) => suspendBox(Z, S(q))(Box(a, cn), shift)
    case (S(p: P), Z, Dot(a, _), shift) => Dot(shift(a), TypeLemmas.addNat(S(p), Z))
    case (S(p: P), Z, Box(a, cn), shift) => suspendBox(S(p), Z)(Box(a, cn), shift)
    case (S(p: P), S(q: Q), Dot(a, _), shift) => Dot(shift(a), TypeLemmas.addNat(S(p), S(q)))
    case (S(p: P), S(q: Q), Box(a, cn), shift) => suspendBox(S(p), S(q))(Box(a, cn), shift)
  }

}

object Nesting extends NestingFunctions 
