/**
  * TypeCheckers.scala - Typechecker for Baez-Dolan style opetopic categories
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.bd

import scalaz.Monad
import scalaz.MonadReader

import scalaz.EitherT
import scalaz.ReaderT
import scalaz.Kleisli
import scalaz.{\/, -\/, \/-}

import scalaz.syntax.either
import scalaz.syntax.kleisli

import opetopic._

object TypeChecker {

  type TypeEnv = Map[String, Int]

  type TypeErrorM[A] = EitherT[ShapeM, ShapeError, A]
  type CheckerR[T, A] = ReaderT[TypeErrorM, T, A]
  type CheckerM[A] = ReaderT[TypeErrorM, TypeEnv, A]

  val R = MonadReader[CheckerR, TypeEnv]
  import R._


}
