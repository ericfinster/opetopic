/**
  * SimpleMarker.scala - A Generic Marker 
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui.markers

import opetopic._
import opetopic.ui._
import syntax.tree._

sealed trait SimpleMarker[N <: Nat] {

  def label: String
  def colorSpec: ColorSpec

  def visualize(frmwk: UIFramework) : frmwk.Visualization[N]
  
}

case class SimpleObjectMarker(
  val label: String,
  val colorSpec: ColorSpec = DefaultColorSpec
) extends SimpleMarker[_0] {

  def visualize(frmwk: UIFramework) : frmwk.Visualization[_0] =
    frmwk.ObjectVisualization(colorSpec, frmwk.text(label))

}

case class SimpleCellMarker[P <: Nat](
  val label: String,
  val colorSpec: ColorSpec = DefaultColorSpec,
  val rootEdgeDecoration: TriangleDec = Nonexistant,
  val leafEdgeDecorations: Option[Tree[TriangleDec, P]] = None
) extends SimpleMarker[S[P]] {

  def visualize(frmwk: UIFramework) : frmwk.Visualization[S[P]] = {

    import frmwk._
    import isNumeric._

    implicit def intToUnit(i: Int) : Size =
      fromInt(i)

    def lblEl(lbl: String) =
      if (lbl == "") spacer(Bounds(0,0,600,600)) else text(lbl)

    def triUp(c: String) : PolygonType = polygon(c, 100, c, List((150, 0), (300, 300), (0, 300)))
    def triDown(c: String) : PolygonType = polygon(c, 100, c, List((0, 0), (150, 300), (300, 0)))
    def bnds : Bounds = Bounds(0, 0, 300, 300)

    def renderUp(d: TriangleDec) : Option[BoundedElement] =
      d match {
        case Nonexistant => None
        case BlackTriangle => Some(BoundedElement(triUp("black"), bnds))
        case RedTriangle => Some(BoundedElement(triUp("red"), bnds))
      }

    def renderDown(d: TriangleDec) : Option[BoundedElement] =
      d match {
        case Nonexistant => None
        case BlackTriangle => Some(BoundedElement(triDown("black"), bnds))
        case RedTriangle => Some(BoundedElement(triDown("red"), bnds))
      }

    val markRoot = renderDown(rootEdgeDecoration)
    val markLeaves = leafEdgeDecorations map ((tr : Tree[TriangleDec, P]) => {
      tr map ((t: TriangleDec) => renderUp(t))
    })

    frmwk.CellVisualization(colorSpec, lblEl(label), markRoot, markLeaves)

  }

}

object SimpleMarker {

  type OptMarker[N <: Nat] = Option[SimpleMarker[N]]

  @natElim
  def apply[N <: Nat](n: N)(lbl: String, spec: ColorSpec) : SimpleMarker[N] = {
    case (Z, lbl, spec) => SimpleObjectMarker(lbl, spec)
    case (S(p), lbl, spec) => SimpleCellMarker(lbl, spec)
  }

  def frameworkFamily(frmwk: UIFramework) : frmwk.VisualizableFamily[SimpleMarker] = 
    new frmwk.VisualizableFamily[SimpleMarker] {
      def visualize[N <: Nat](n: N)(m: SimpleMarker[N]) = 
        m.visualize(frmwk)
    }

  //============================================================================================
  // PICKLING
  //

  import scala.{PartialFunction => PF}

  import upickle.Js
  import upickle.default._
  import Pickler.IndexedReader
  import Pickler.IndexedWriter

  implicit def simpleMarkerWriter : IndexedWriter[SimpleMarker] = 
    new IndexedWriter[SimpleMarker] {

      @natElim
      def apply[N <: Nat](n: N) : Writer[SimpleMarker[N]] = {
        case Z => 
          new Writer[SimpleMarker[_0]] {
            def write0: SimpleMarker[_0] => Js.Value = {
              case SimpleObjectMarker(lbl, cs) => {
                val csJson = implicitly[Writer[ColorSpec]].write(cs)
                Js.Obj(("type", Js.Str("som")), ("lbl", Js.Str(lbl)), ("cs", csJson))
              }
            }
          }
        case S(p: P) =>
          new Writer[SimpleMarker[S[P]]] {
            def write0: SimpleMarker[S[P]] => Js.Value = {
              case SimpleCellMarker(lbl, cs, re, None) => {
                val csJson = implicitly[Writer[ColorSpec]].write(cs)
                val reJson = implicitly[Writer[TriangleDec]].write(re)
                Js.Obj(
                  ("type", Js.Str("scm")), 
                  ("lbl", Js.Str(lbl)),
                  ("cs", csJson),
                  ("re", reJson),
                  ("le", Js.Null)
                )
              }
              case SimpleCellMarker(lbl, cs, re, Some(le)) => {
                val csWriter = implicitly[Writer[ColorSpec]]
                val decWriter = implicitly[Writer[TriangleDec]]
                Js.Obj(
                  ("type", Js.Str("scm")),
                  ("lbl", Js.Str(lbl)),
                  ("cs", csWriter.write(cs)),
                  ("re", decWriter.write(re)),
                  ("le", Tree.treeWriter(decWriter).write(le))
                )
              }
            }
          }
      }
    }


  implicit def simpleMarkerReader : IndexedReader[SimpleMarker] = 
    new IndexedReader[SimpleMarker] {

      @natElim
      def apply[N <: Nat](n: N) : Reader[SimpleMarker[N]] = {
        case Z => 
          new Reader[SimpleMarker[_0]] {
            def read0: PF[Js.Value, SimpleMarker[_0]] = {
              case Js.Obj(("type", Js.Str("som")), ("lbl", Js.Str(lbl)), ("cs", csJson)) => {
                val cs = implicitly[Reader[ColorSpec]].read(csJson)
                SimpleObjectMarker(lbl, cs)
              }
            }
          }
        case S(p: P) =>
          new Reader[SimpleMarker[S[P]]] {
            def read0: PF[Js.Value, SimpleMarker[S[P]]]= {
              case Js.Obj(
                ("type", Js.Str("scm")), ("lbl", Js.Str(lbl)), 
                ("cs", csJson), ("re", reJson), ("le", leJson)
              ) => {
                val cs = implicitly[Reader[ColorSpec]].read(csJson)
                val decReader = implicitly[Reader[TriangleDec]]
                val re = decReader.read(reJson)
                val le = 
                  leJson match {
                    case Js.Null => None
                    case lj => Some(Tree.treeReader(decReader, p).read(lj))
                  }
                SimpleCellMarker(lbl, cs, re, le)
              }
            }
          }
      }

    }

}
