/**
  * Examples.scala - Some examples and testing
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.core

import scala.language.implicitConversions

import Nats._
import Complex._
import ComplexZipper._

object Examples {

  val fred4 : Nesting[_4, Int] = Dot(27, __4)

  val fred3 : Nesting[_3, Int] = Box(26, Node(Dot(25, __3),Node(Node(Dot(24, __3),Node(Leaf(__3),Node(Leaf(__2),Pt(Node(Node(Leaf(__3),Node(Node(Leaf(__3),Leaf(__1)),Pt(Node(Leaf(__2),Pt(Leaf(__1)))))),Pt(Node(Leaf(__2),Pt(Leaf(__1))))))))),Node(Node(Leaf(__3),Node(Leaf(__2),Pt(Leaf(__1)))),Pt(Node(Leaf(__2),Pt(Node(Node(Node(Dot(23, __3),Node(Leaf(__3),Node(Leaf(__2),Pt(Leaf(__1))))),Node(Node(Leaf(__3),Node(Leaf(__2),Pt(Leaf(__1)))),Pt(Leaf(__1)))),Pt(Leaf(__1))))))))))

  val fred2 : Nesting[_2, Int] = Box(22, Node(Box(21, Node(Dot(18, __2),Node(Leaf(__2),Pt(Node(Node(Dot(17, __2),Node(Node(Dot(16, __2),Leaf(__1)),Pt(Node(Leaf(__2),Pt(Leaf(__1)))))),Pt(Node(Leaf(__2),Pt(Leaf(__1))))))))),Node(Node(Dot(19, __2),Node(Leaf(__2),Pt(Leaf(__1)))),Pt(Node(Leaf(__2),Pt(Node(Node(Box(20, Node(Dot(15, __2),Node(Leaf(__2),Pt(Leaf(__1))))),Node(Node(Dot(14, __2),Node(Leaf(__2),Pt(Leaf(__1)))),Pt(Leaf(__1)))),Pt(Leaf(__1)))))))))

  val fred1 : Nesting[_1, Int] = Box(13, Node(Box(12, Node(Dot(7, __1),Pt(Leaf(__1)))),Pt(Node(Box(11, Node(Box(10, Leaf(__1)),Pt(Node(Dot(6, __1),Pt(Leaf(__1)))))),Pt(Node(Box(9, Node(Box(8, Node(Dot(5, __1),Pt(Leaf(__1)))),Pt(Leaf(__1)))),Pt(Leaf(__1))))))))

  val fred0 : Nesting[_0, Int] = Box(4, Pt(Box(3, Pt(Box(2, Pt(Obj(1)))))))

  val fredC : Complex[_3, Int] = EmptyC :>> fred0 :>> fred1 :>> fred2 :>> fred3
  val fredZ : ComplexZipper[_3, Int] = fromComplex(fredC)

  // val fred3Addr : Nesting[_3, (Int, Address[_4])] = Nesting.withAddress(fred3)
  // def dumpAddresses : Unit = {
  //   println(fred3Addr.toString)
  // }

  val Addr26 = Root()(S(S(S(S(Z)))))
  val Addr25 = Step(Dir(Root()(S(S(S(Z))))),Root()(S(S(S(S(Z))))))
  val Addr24 = Step(Dir(Step(Dir(Root()(S(S(Z)))),Root()(S(S(S(Z)))))),Root()(S(S(S(S(Z))))))
  val Addr23 = Step(Dir(Step(Dir(Step(Dir(Step(Dir(Root()(Z)),Step(Dir(Root()(Z)),Root()(S(Z))))),Root()(S(S(Z))))),Root()(S(S(S(Z)))))),Root()(S(S(S(S(Z))))))

  // val test : Option[ComplexZipper[_3, Int]] = seekComplex(Addr23, fredZ)

  def doFlatten[N <: Nat, A](nst : Nesting[N, A]) : Option[Tree[N, Address[S[N]]]] = 
    Tree.flatten(Nesting.toTree(nst))

}
