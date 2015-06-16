/**
  * FXSvgGenerator.scala - Base Trait for Viewers with SVG support
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.fx

// import scala.language.higherKinds

// import opetopic._
// import opetopic.ui._
// import TypeDefs._
// import syntax.tree._
// import syntax.complex._

// object FXSVGBundle 
//     extends scalatags.Text.Cap
//     with scalatags.text.SvgTags
//     with scalatags.DataConverters
//     with scalatags.Text.Aggregate{
//       object svgattr extends scalatags.Text.Cap with scalatags.Text.SvgAttrs
//     }

// import FXSVGBundle._

// trait FXSvgGenerator[A[_ <: Nat]] { thisGen : Viewer[A, Double] =>

//   def toSvg : Tag = {
//     import svgattr._
//     svg(width:=200,height:=200,xmlns:="http://www.w3.org/2000/svg")(complexToSvg(complex.value))
//   }

//   def complexToSvg[N <: Nat](cmplx: Complex[MarkerType, N]) : Seq[Tag] = Seq()
//     // (new NatCaseSplit0 {

//     //   type Out[N <: Nat] = Complex[MarkerType, N] => Seq[Tag]

//     //   def caseZero : Out[_0] = {
//     //     case Complex(_, hd) => {

//     //       def renderObject(mk: MarkerType[_0]) : Tag = {
//     //         import svgattr._
//     //         rect(x:=mk.x,y:=mk.y,width:=mk.width,height:=mk.height)
//     //       }

//     //       def genObjectNesting(nst: Nesting[MarkerType[_0], _0]) : Seq[Tag] = 
//     //         nst match {
//     //           case Obj(mk) => Seq(renderObject(mk))
//     //           case Box(mk, Pt(n)) => {
//     //             genObjectNesting(n) :+ renderObject(mk)
//     //           }
//     //         }

//     //       Seq(g(genObjectNesting(hd)))
//     //     }
//     //   }

//     //   def caseSucc[P <: Nat](p: P) : Out[S[P]] = {
//     //     case Complex(tl, hd) => {
//     //       val tailGroups = this(p)(tl)

//     //       def renderBox(mk: MarkerType[S[P]]) : Tag = {
//     //         import svgattr._
//     //         rect(x:=mk.x,y:=mk.y,width:=mk.width,height:=mk.height)
//     //       }

//     //       def genNesting(nst: Nesting[MarkerType[S[P]], S[P]]) : Seq[Tag] =
//     //         nst match{
//     //           case Dot(mk, _) => Seq(renderBox(mk))
//     //           case Box(mk, cn) => {
//     //             (cn.map(genNesting(_))).nodes.flatten :+ renderBox(mk)
//     //           }
//     //         }

//     //       tailGroups :+ g(genNesting(hd))
//     //     }
//     //   }

//     // })(cmplx.length.pred)(cmplx)

// }
