/**
  * Examples.scala - Some Example Trees and Nestings and Complexes
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic

object Examples {
  
  val fred0 : Nesting[Int, _0] = 
    Box(0,Pt(Box(1,Pt(Box(2,Pt(Obj(3)))))))

  val fred1 : Nesting[Int, _1] = 
    Box(4,Node(Box(5,Node(Box(6,Node(Dot(7,S(Z)),Pt(Leaf(S(Z))))),Pt(Node(Box(8,Node(Dot(9,S(Z)),Pt(Node(Box(10,Leaf(S(Z))),Pt(Leaf(S(Z))))))),Pt(Leaf(S(Z))))))),Pt(Node(Box(11,Node(Dot(12,S(Z)),Pt(Leaf(S(Z))))),Pt(Leaf(S(Z)))))))

  val fred2 : Nesting[Int, _2] = 
    Box(13,Node(Box(14,Node(Dot(15,S(S(Z))),Node(Leaf(S(S(Z))),Pt(Node(Node(Dot(16,S(S(Z))),Node(Leaf(S(S(Z))),Pt(Leaf(S(Z))))),Pt(Leaf(S(Z)))))))),Node(Node(Box(17,Node(Dot(18,S(S(Z))),Node(Leaf(S(S(Z))),Pt(Node(Leaf(S(S(Z))),Pt(Leaf(S(Z)))))))),Node(Node(Dot(19,S(S(Z))),Node(Leaf(S(S(Z))),Pt(Leaf(S(Z))))),Pt(Node(Node(Dot(20,S(S(Z))),Node(Leaf(S(S(Z))),Pt(Node(Node(Dot(21,S(S(Z))),Leaf(S(Z))),Pt(Leaf(S(Z))))))),Pt(Leaf(S(Z))))))),Pt(Node(Node(Box(22,Leaf(S(S(Z)))),Node(Leaf(S(S(Z))),Pt(Leaf(S(Z))))),Pt(Leaf(S(Z))))))))

  val fred3 : Nesting[Int, _3] = 
    Box(23,Node(Dot(24,S(S(S(Z)))),Node(Node(Dot(25,S(S(S(Z)))),Node(Leaf(S(S(S(Z)))),Node(Leaf(S(S(Z))),Pt(Node(Node(Leaf(S(S(S(Z)))),Node(Leaf(S(S(Z))),Pt(Leaf(S(Z))))),Pt(Leaf(S(Z)))))))),Node(Node(Node(Dot(26,S(S(S(Z)))),Node(Leaf(S(S(S(Z)))),Node(Leaf(S(S(Z))),Pt(Node(Leaf(S(S(Z))),Pt(Leaf(S(Z)))))))),Node(Node(Leaf(S(S(S(Z)))),Node(Leaf(S(S(Z))),Pt(Leaf(S(Z))))),Pt(Node(Node(Leaf(S(S(S(Z)))),Node(Leaf(S(S(Z))),Pt(Node(Node(Leaf(S(S(S(Z)))),Leaf(S(Z))),Pt(Leaf(S(Z))))))),Pt(Leaf(S(Z))))))),Pt(Node(Node(Node(Dot(27,S(S(S(Z)))),Leaf(S(S(Z)))),Node(Leaf(S(S(Z))),Pt(Leaf(S(Z))))),Pt(Leaf(S(Z)))))))))

  val fred4 : Nesting[Int, _4] = 
    Dot(28,S(S(S(S(Z)))))

  val fredComplex : Complex[ConstInt, _4] = 
    Complex[ConstInt] >> fred0 >> fred1 >> fred2 >> fred3 >> fred4

  val exotic : Nesting[String, _2] = 
    Box("\u03b1 \u2297 \u03b2",Node(Box("B",Node(Dot("univ-adj-\u03b3",S(S(Z))),Node(Leaf(S(S(Z))),Pt(Node(Node(Dot("D",S(S(Z))),Node(Leaf(S(S(Z))),Pt(Leaf(S(Z))))),Pt(Leaf(S(Z)))))))),Node(Node(Box("E",Node(Dot("F",S(S(Z))),Node(Leaf(S(S(Z))),Pt(Node(Leaf(S(S(Z))),Pt(Leaf(S(Z)))))))),Node(Node(Dot("\u03b1-coh",S(S(Z))),Node(Leaf(S(S(Z))),Pt(Leaf(S(Z))))),Pt(Node(Node(Dot("I",S(S(Z))),Node(Leaf(S(S(Z))),Pt(Node(Node(Dot("H \u229b V",S(S(Z))),Leaf(S(Z))),Pt(Leaf(S(Z))))))),Pt(Leaf(S(Z))))))),Pt(Node(Node(Box("\u0398",Leaf(S(S(Z)))),Node(Leaf(S(S(Z))),Pt(Leaf(S(Z))))),Pt(Leaf(S(Z))))))))


}
