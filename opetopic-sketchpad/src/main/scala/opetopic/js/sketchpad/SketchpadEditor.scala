// /**
//   * SketchpadEditor.scala - Custom Editor for the Sketchpad
//   * 
//   * @author Eric Finster
//   * @version 0.1 
//   */

// package opetopic.js.sketchpad

// import org.scalajs.dom
// import org.scalajs.jquery._

// import opetopic._
// import opetopic.js._
// import opetopic.ui._
// import syntax.complex._
// import syntax.nesting._
// import syntax.tree._
// import markers._
// import JsDomFramework._
// import SimpleMarker._

// class SketchpadEditor extends JsCardinalEditor[SimpleMarker] {

//   implicit val vf: VisualizableFamily[SimpleMarker] = 
//     SimpleMarker.frameworkFamily(JsDomFramework)

//   //============================================================================================
//   // SELECTION HANDLERS
//   //

//   @natElim
//   override def onSelect[N <: Nat](n: N)(editor: CardinalEditor[SimpleMarker])(box: editor.CardinalCellBox[N]): Unit = {
//     case (Z, editor, box) => {
//       Sketchpad.showProps(box.optLabel)
//       showFace
//     }
//     case (S(p: P), editor, box) => {
//       for {
//         lc <- box.labelComplex
//       } {

//         implicit val v : Visualizable[OptMarker[P], P] =
//           new Visualizable[OptMarker[P], P] {
//             def visualize(m: OptMarker[P]) : Visualization[P] =
//               Sketchpad.viewer.vf.visualize(p)(m)
//           }

//         val frameNesting = lc.tail.head

//         val panel = ActivePanel(frameNesting)
//         panel.refresh
//         panel.setupViewport
//         // panel.viewport.width = 200
//         // panel.viewport.height = 200
//         // panel.viewport.setBounds(panel.bounds)

//         def defaultLeafDecs : Tree[TriangleDec, P] =
//           frameNesting match {
//             case Box(bv, cn) => cn map (_ => Nonexistant)
//             case _ => throw new IllegalArgumentException("Malformed complex")
//           }

//         panel.onBoxClicked = { (b: SimpleActiveCellBox[OptMarker[P], P]) => {

//           val curMk : SimpleCellMarker[P] =
//             box.optLabel match {
//               case None => SimpleCellMarker("", DefaultColorSpec, Nonexistant, Some(defaultLeafDecs))
//               case Some(SimpleCellMarker(l, s, r, None)) => SimpleCellMarker(l, s, r, Some(defaultLeafDecs))
//               case Some(mk @ SimpleCellMarker(l, s, r, Some(_))) => mk
//             }

//           if (b.isExternal) {

//             for {
//               lds <- fromOpt(curMk.leafEdgeDecorations)
//               zp <- lds.seekTo(b.address.head)
//               d <- zp._1.rootValue
//             } {

//               val newLds = Zipper.close(p)(zp._2, zp._1.withRootValue(d.next))
//               box.optLabel = Some(curMk.copy(leafEdgeDecorations = Some(newLds)))
//             }

//           } else { box.optLabel = Some(curMk.copy(rootEdgeDecoration = curMk.rootEdgeDecoration.next)) }

//           box.panel.refresh
//           Sketchpad.editor.refreshEditor
//           showFace

//         }}

//         jQuery("#edge-props").empty().append(panel.element.uiElement)

//         Sketchpad.showProps(box.optLabel)
//         showFace
//       }
//     }
//   }

//   def showFace: Unit = 
//     for {
//       instance <- activeEditor
//       boxsig <- instance.rootBox
//       lc <- boxsig.value.labelComplex
//     } { Sketchpad.viewer.complex = Some(lc) }


// }
