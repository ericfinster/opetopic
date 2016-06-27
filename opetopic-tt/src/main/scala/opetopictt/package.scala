/**
  * package.scala - Globals for Scala OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package object opetopictt {

  type Ident = String
  type Name = String
  type CaseTk = ((Int, Int), String)
  type DataTk = ((Int, Int), String)
  type RecTk = ((Int, Int), String)
  type CofunTk = ((Int, Int), String)
  type Pos = ((Int, Int), String)
  type SClos = (List[(Name, Expr)], Rho)
  type   Nf  = Val
  type TVal  = Val

  type Gamma = List[(Name, TVal)]
  type Tele = List[(Name, Expr)]

}
