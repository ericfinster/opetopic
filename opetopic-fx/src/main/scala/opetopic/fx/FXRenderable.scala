/**
  * FXRenderable.scala - Renderable Type Class
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx

import scala.language.higherKinds

import javafx.scene.Node
import javafx.scene.text.Text
import javafx.scene.paint.Color
import javafx.scene.shape.Rectangle

import opetopic._
import TypeDefs._

trait FXRenderable[A[_ <: Nat]] {
  def render[N <: Nat](n: N)(an: A[N]) : Node
}

object FXRenderable {

  implicit def constIntIsRenderable : FXRenderable[ConstInt] = 
    new FXRenderable[ConstInt] {
      def render[N <: Nat](n: N)(i : Int) = new Text(i.toString)
    }

  implicit def constStringIsRenderable : FXRenderable[ConstString] = 
    new FXRenderable[ConstString] {
      def render[N <: Nat](n: N)(str : String) = new Text(str)
    }

  implicit def optIsRenderable[A[_ <: Nat]](implicit fxr: FXRenderable[A]) : FXRenderable[({ type L[K <: Nat] = Option[A[K]] })#L] = 
    new FXRenderable[({ type L[K <: Nat] = Option[A[K]] })#L] {
      def render[N <: Nat](n: N)(opt: Option[A[N]]) : Node = 
        opt match {
          case Some(an) => fxr.render(n)(an)
          case None => {
            val rect = new Rectangle(10.0, 10.0)
            rect.setFill(Color.TRANSPARENT)
            rect
          }
        }
    }

  implicit def polarityIsRenderable[A[_ <: Nat]](implicit fxr: FXRenderable[A]) : FXRenderable[({ type L[K <: Nat] = Polarity[A[K]] })#L] = 
    new FXRenderable[({ type L[K <: Nat] = Polarity[A[K]] })#L] {
      def render[N <: Nat](n: N)(pol: Polarity[A[N]]) : Node = 
        pol match {
          case Positive() => new Text("+")
          case Negative() => new Text("-")
          case Neutral(an) => fxr.render(n)(an)
        }
    }

}
