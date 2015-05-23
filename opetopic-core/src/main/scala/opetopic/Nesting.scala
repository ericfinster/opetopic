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


  // Here is a first example of the expansion problem you knew you would face: rewriting in 
  // the nesting zipper guy here produces an illegal cyclic reference which you are going to
  // need to avoid somehow.

  // The idea, of course, is to keep a black-list of types which should be unfolded by hand
  // and thereby avoid writing the type in its reducible form.

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

}

object Nesting extends NestingFunctions {

  import upickle._

  implicit def nestingWriter[A, N <: Nat](implicit wrtr: Writer[A]) : Writer[Nesting[A, N]] = 
    new Writer[Nesting[A, N]] {
      def write0: Nesting[A, N] => Js.Value = {
        case Obj(a) => Js.Obj(("type", Js.Str("obj")), ("val", wrtr.write(a)))
        case Dot(a, d) => Js.Obj(("type", Js.Str("dot")), ("val", wrtr.write(a)))
        case Box(a, cn) => {
          val canopyWriter : Writer[Tree[Nesting[A, N], N]] = 
            Tree.treeWriter(this)
          Js.Obj(("type", Js.Str("box")), ("val", wrtr.write(a)), ("canopy", canopyWriter.write(cn)))
        }
      }
    }

  @natElim
  implicit def nestingReader[A, N <: Nat](n: N)(implicit rdr: Reader[A]) : Reader[Nesting[A, N]] = {
    case Z => {
      new Reader[Nesting[A, _0]] { thisRdr =>
        def read0: PartialFunction[Js.Value, Nesting[A, _0]] = {
          case Js.Obj(("type", Js.Str("obj")), ("val", a)) => Obj(rdr.read(a))
          case Js.Obj(("type", Js.Str("box")), ("val", a), ("canopy", cn)) => {
            val canopyReader : Reader[Tree[Nesting[A, _0], _0]] =
              Tree.treeReader(Z)(thisRdr)
            Box(rdr.read(a), canopyReader.read(cn))
          }
        }
      }
    }
    case S(p) => {
      new Reader[Nesting[A, S[Nat]]] { thisRdr =>
        def read0: PartialFunction[Js.Value, Nesting[A, S[Nat]]] = {
          case Js.Obj(("type", Js.Str("dot")), ("val", a)) => Dot(rdr.read(a), S(p))
          case Js.Obj(("type", Js.Str("box")), ("val", a), ("canopy", cn)) => {
            val canopyReader : Reader[Tree[Nesting[A, S[Nat]], S[Nat]]] =
              Tree.treeReader(S(p))(thisRdr)
            Box(rdr.read(a), canopyReader.read(cn))
          }
        }
      }
    }
  }

}

