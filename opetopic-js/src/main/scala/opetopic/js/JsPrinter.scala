/**
  * JsPrinter.scala - Pretty Printing for testing OpetopicTT
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import opetopic._
import syntax.tree._
import syntax.complex._
import syntax.nesting._

import scalaz.\/
import scalaz.-\/
import scalaz.\/-

object JsPrinter {

  //============================================================================================
  // PRETTY PRINT
  //

  @natElim
  def prettyPrintAddress[N <: Nat](n: N)(addr: Address[N]) : String = {
    case (Z, ()) => "#"
    case (S(p), Nil) => "*"
    case (S(p), a :: as) => 
      prettyPrintAddress(p)(a) ++ " > " ++ 
        prettyPrintAddress(S(p))(as)
  }

  def printTuple(ls: List[String]) : String = 
    ls match {
      case Nil => ""
      case s :: Nil => s
      case s :: t :: ss => printTuple(("(" ++ s ++ ", " ++ t ++ ")") :: ss)

    }

  def prettyPrintNesting[N <: Nat](nst: Nesting[List[String], N]) : String = 
    nst match {
      case Obj(s) => "leaf i"
      case Dot(s, d) => "leaf " ++ printTuple(s.reverse)
      case Box(s, cn) => {

        val dim = cn.dim

        val brs = cn.mapWithAddress({
          case (n, addr) => prettyPrintAddress(dim)(addr) ++ " => " ++ prettyPrintNesting(n)
        }).nodes

        "[ " ++ s.head ++ " | " ++ brs.mkString(" | ") ++ " ]"

      }
    }

  @natElim 
  def prettyPrintComplex[A[_ <: Nat], N <: Nat](n: N)(cmplx: Complex[A, N]) : List[String] = {
    case (Z, Complex(_, hd)) => List("i")
    case (S(p: P), Complex(Complex(_, objs), ars)) => List("eta i", "i")
    case (S(S(p: P)), cc @ Complex(cm, _)) => {

      val calc : ShapeM[String] =
        for {
          dbl <- cm.comultiply
          sourceTree <- dbl.headSpine
          printedSource = sourceTree map (prettyPrintComplex(S(p))(_))
          spine <- dbl.tail.headSpine
          printedSpine = spine map (prettyPrintComplex(p)(_))
          nst <- Nesting.fromTree(printedSource)
          res <- nst.traverse({
            case -\/(ls) => succeed(ls)
            case \/-(addr) => Tree.valueAt(printedSpine, addr)
          })
        } yield prettyPrintNesting(res)

      val thisDim : String = 
        calc match {
          case -\/(_) => "fail"
          case \/-(s) => s
        }

      val tgts : List[String] = 
        cc.target match {
          case -\/(_) => List("fail")
          case \/-(tgt) => prettyPrintComplex(S(p))(tgt)
        }

      thisDim :: tgts

    }
  }

}



