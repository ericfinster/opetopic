/**
  * package.scala - Globals for Scala MiniTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

package object newtt {

  type Ident = String
  type Name = String
  type SClos = (List[(Name, Expr)], Rho)
  type Nf = Val
  type TVal = Val

}
