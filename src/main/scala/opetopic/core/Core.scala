/**
  * Core.scala - Package Object for Core Package
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

package object core {

  // Package wide type aliases should go here
  type Complex[N <: Nat, +A] = ConsSeq[Nesting, S[N], A]


}
