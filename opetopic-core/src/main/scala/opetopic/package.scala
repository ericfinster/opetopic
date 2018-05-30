/**
  * package.scala - Package object for stable trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package object opetopic extends ComplexTypes with CardinalTypes {

  type SAddr = List[SDir]
  type Shell[A] = STree[STree[A]]
  type TShell[A] = STree[Shell[A]]
  type QShell[A] = STree[TShell[A]]

  
  //============================================================================================
  // LAZY TRAVERSAL HELPER
  //

  trait LazyTraverse[G[_], A, B, C] {
    def apply(a: A, addr: => SAddr, der: => SDeriv[B]) : G[C]
  }

  implicit def funcToLt[G[_], A, B](f: A => G[B]) : LazyTraverse[G, A, Unit, B] =
    new LazyTraverse[G, A, Unit, B] { 
      def apply(a: A, addr: => SAddr, der: => SDeriv[Unit]) = f(a)
    }

  implicit def funcAddrToLt[G[_], A, B](f: (A, => SAddr) => G[B]): LazyTraverse[G, A, Unit, B] = 
    new LazyTraverse[G, A, Unit, B] { 
      def apply(a: A, addr: => SAddr, der: => SDeriv[Unit]) = f(a, addr)
    }

  implicit def funcDerivToLt[G[_], A, B, C](f: (A, => SDeriv[B]) => G[C]): LazyTraverse[G, A, B, C] = 
    new LazyTraverse[G, A, B, C] {
      def apply(a: A, addr: => SAddr, der: => SDeriv[B]) = f(a, der)
    }

  implicit def funcAddrDerivToLt[G[_], A, B, C](f: (A, => SAddr, => SDeriv[B]) => G[C]): LazyTraverse[G, A, B, C] = 
    new LazyTraverse[G, A, B, C] {
      def apply(a: A, addr: => SAddr, der: => SDeriv[B]) = f(a, addr, der)
    }

  //============================================================================================
  // POLARITIES
  //

  sealed trait Polarity[+A]
  sealed trait Polarization[+A] extends Polarity[A]
  case class Positive[+A]() extends Polarization[A] { override def toString = "+" }
  case class Negative[+A]() extends Polarization[A] { override def toString = "-" }
  case class Neutral[+A](a : A) extends Polarity[A] { override def toString = a.toString }

  //============================================================================================
  // FACETS
  //

  // Represents a face together with a choice of
  // facet, i.e. target or source in a given direction.

  sealed trait Facet[A] {
    val face: A
    def withFace[B](b: B): Facet[B]
    def negate(d: SDir = SDir(Nil)): Facet[A]
    val isSrc: Boolean
  }

  case class SrcFacet[A](val face: A, dir: SDir) extends Facet[A] {
    val isSrc = true
    def withFace[B](b: B) = SrcFacet(b, dir)
    def negate(d: SDir) = TgtFacet(face)
    override def toString = "- " ++ face.toString
  }

  case class TgtFacet[A](val face: A) extends Facet[A] {
    val isSrc = false
    def withFace[B](b: B) = TgtFacet(b)
    def negate(d: SDir) = SrcFacet(face, d)
    override def toString = "+ " ++ face.toString
  }

  //============================================================================================
  // FLAGS THEIR ZIPPERS
  //

  type Flag[A] = List[Facet[A]]
  type FlagZipper[A] = Flag[SNstZipper[A]]

  def flagStr[A](f: Flag[A]) =
    f.map(_.toString).mkString(" ")
  
}
