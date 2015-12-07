/**
  * EditorEnvironment.scala - Encapsulate the Environment
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.codebuilder

import opetopic._
import opetopic.tt._

import scalaz.\/

import OpetopicTypeChecker._

abstract class EditorEnvironment {

  val catExpr : Expr = EVar("X")

  var rho : Rho = UpVar(RNil, PVar("X"), Nt(Gen(0, "TC#")))
  var gma : Gamma = ("X", Cat) :: Nil

  def genV = Nt(Gen(lRho(rho), "TC#"))

  def extendContext(id: String, ty: Val): Unit = 
    gma = (id, ty) :: gma

  def extendEnvironment(id: String, v: Val): Unit = 
    rho = UpVar(rho, PVar(id), v)

  def eval(e: Expr) : Val = 
    OpetopicTypeChecker.eval(e, rho)

  def checkT(e: Expr) : G[Unit] = 
    OpetopicTypeChecker.checkT(rho, gma, e)

  def check(e: Expr, v: Val) : G[Unit] = 
    OpetopicTypeChecker.check(rho, gma, e, v)

  def registerCell[N <: Nat](cell: Cell[N]) : Unit 
  def registerProperty[N <: Nat](prop: Property[N]) : Unit

  def registerParameter[N <: Nat](cell : Cell[N]) : Unit
  def registerParameter[N <: Nat](prop : Property[N]) : Unit

}

