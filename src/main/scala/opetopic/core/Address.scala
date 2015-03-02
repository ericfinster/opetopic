/**
  * Address.scala - Tree Addresses
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.implicitConversions

import Nats._

sealed trait Direction[N <: Nat] { def dim : N }
case class Dir[N <: Nat](addr : Address[N]) extends Direction[S[N]] { def dim = S(addr.dim) }

object Direction {

  implicit def succDirIsAddress[N <: Nat](dir : Direction[S[N]]) : Address[N] = 
    dir match {
      case Dir(addr) => addr
    }

  implicit def addrIsSuccDirction[N <: Nat](addr : Address[N]) : Direction[S[N]] = 
    Dir(addr)

}

sealed trait Address[N <: Nat] { 
  
  def dim : N 

  override def toString : String =
    dim match {
      case IsZero(zm) => "[|]"
      case IsOne(om) => {

        def toInt(addr : Address[_1]) : Int = 
          addr match {
            case Root() => 0
            case Step(_, prev) => toInt(prev) + 1
          }

        toInt(om.oneCoh.subst(this)).toString
      }
      case IsDblSucc(dm) => 
        dm.dblSuccCoh.subst(this) match {
          case Root() => "[|]"
          case Step(Dir(d), ds) => "(" ++ d.toString ++ ") :: " ++ ds.toString
        }
    }

}

case class Root[N <: Nat](implicit val n : N) extends Address[N] { def dim = n }
case class Step[N <: Nat](dir : Direction[N], addr : Address[N]) extends Address[N] { def dim = addr.dim }


