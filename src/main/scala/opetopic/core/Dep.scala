/**
  * Dep.scala - Simple dependent pattern matching setup
  * 
  * @author Eric Finster
  * @version 0.1 
  */

// Based on https://gist.github.com/88237c53916f8357b894.git

package opetopic.core

import scala.language.higherKinds
import Nats._

trait Dep[T]

trait Pack[T] {
  val dep: Dep[T]
  val t: T
}

object Pack {

  def apply[T](d: Dep[T], t0: T) =
    new Pack[T] { val dep = d; val t = t0 }

  def unapply[T](p: Pack[T]): Option[(Dep[T], T)] = Some(p.dep, p.t)

}

