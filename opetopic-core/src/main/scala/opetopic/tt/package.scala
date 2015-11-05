/**
  * package.scala - Package object for OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

package object tt {

  type Ident = String
  type Name = String
  type Pos = ((Int, Int), String)
  type SClos = (List[(Name, Expr)], Rho)
  type Nf = Val
  type TVal = Val

}
