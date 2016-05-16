/**
  * package.scala - Package object for stable trees
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

package object stable extends ComplexTypes {

  type SAddr = List[SDir]
  type Shell[A] = STree[STree[A]]

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

}
