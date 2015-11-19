/**
  * PPrint.scala - Base definitions for pretty printing
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.pprint

import scala.{Iterator => Iter}
import opetopic._

case class Config(
  width: Int = 80,
  depth: Int = 0,
  indent: Int = 2
) { def deeper = copy(depth = depth + 1) }

trait Chunker[T] {
  def chunk(t: T, c: Config): Iter[Iter[String]]
}

trait PPrint[T] {
  def render(t: T, c: Config): Iter[String]
}

trait PPrintBase {

  def pprint[A](a: A)(implicit c: Config, p: PPrint[A]) : String = 
    p.render(a, c).mkString

  def pprintTree[A, N <: Nat](n: N)(tr: Tree[A, N], c: Config)(implicit ap: PPrint[A]): Iter[String] 
  def pprintNesting[A, N <: Nat](n: N)(nst: Nesting[A, N], c: Config)(implicit ap: PPrint[A]): Iter[String] 

  //============================================================================================
  // DEFAULT CONFIG
  //

  implicit val defaultConfig = Config()


  //============================================================================================
  // IMPLICITS
  //

  implicit object IntPPrint extends PPrint[Int] {
    def render(i: Int, c: Config) = Iter(i.toString)
  }

  implicit def treeIsPPrint[A, N <: Nat](implicit ap: PPrint[A]): PPrint[Tree[A, N]] = 
    new PPrint[Tree[A, N]] {
      def render(t: Tree[A, N], c: Config) = pprintTree(t.dim)(t, c)
    }

  implicit def nestingIsPPrint[A, N <: Nat](implicit ap: PPrint[A]): PPrint[Nesting[A, N]] = 
    new PPrint[Nesting[A, N]] {
      def render(n: Nesting[A, N], c: Config) = pprintNesting(n.dim)(n, c)
    }

}
