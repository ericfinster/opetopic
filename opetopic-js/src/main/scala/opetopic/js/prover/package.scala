/**
  * package.scala - Prover package object
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js

import opetopic._
import opetopic.tt._

import scalaz.\/
import scalaz.\/-
import scalaz.-\/

import scalaz.std.string._

import OpetopicTypeChecker._

package object prover {

  type EditorM[+A] = \/[String, A]

  implicit class EditorOps[A](m: EditorM[A]) {
    def withFilter(f: A => Boolean) : EditorM[A] = 
      m.filter(f)
  }

  def fromShape[A](s: ShapeM[A]) : EditorM[A] =
    s match {
      case -\/(ShapeError(msg)) => -\/("Shape error: " ++ msg)
      case \/-(a) => \/-(a)
    }

  def fromShape[A](s: ShapeM[A], onError : String => String) : EditorM[A] =
    s match {
      case -\/(ShapeError(msg)) => -\/(onError(msg))
      case \/-(a) => \/-(a)
    }

  def toShape[A](m: EditorM[A]) : ShapeM[A] = 
    m match {
      case -\/(msg) => -\/(ShapeError(msg))
      case \/-(a) => \/-(a)
    }

  def attempt[A](opt: Option[A], msg: String) : EditorM[A] =
    opt match {
      case Some(a) => \/-(a)
      case None => -\/(msg)
    }

  def editorError[A](msg: String) : EditorM[A] = -\/(msg)
  def editorSucceed[A](a: A) : EditorM[A] = \/-(a)

  def forceNone[A](opt: Option[A], msg: String) : EditorM[Unit] =
    opt match {
      case None => editorSucceed(())
      case Some(_) => editorError(msg)
    }

  def runCheck[A](m: G[A])(
    onError : String => EditorM[A]
  )(
    onSucceed : A => EditorM[A]
  ) : EditorM[A] =
    m match {
      case -\/(msg) => onError(msg)
      case \/-(a) => onSucceed(a)
    }

  def simpleCheck[A](m: G[A]) : EditorM[A] = 
    runCheck(m)(
      msg => editorError("Typechecking error: " ++ msg)
    )(a => editorSucceed(a))

}
