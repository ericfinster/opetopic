/**
  * SketchpadEditor.scala - Custom Editor for the Sketchpad
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.sketchpad

import org.scalajs.dom
import org.scalajs.jquery._

import opetopic._
import opetopic.js._
import opetopic.ui._
import syntax.complex._
import markers._
import JsDomFramework._

class SketchpadEditor extends JsCardinalEditor[SimpleMarker] {

  implicit val vf: VisualizableFamily[SimpleMarker] = 
    SimpleMarker.frameworkFamily(JsDomFramework)

  //============================================================================================
  // SELECTION HANDLERS
  //

  override def onObjectSelect(editor: CardinalEditor[SimpleMarker])(box: editor.CardinalCellBox[_0]) : Unit = {

    val lblStr = box.optLabel.map(_.label) getOrElse ""
    jQuery("#label-inpu").value(lblStr)

    showFace

  }



  override def onCellSelect[P <: Nat](editor: CardinalEditor[SimpleMarker])(box: editor.CardinalCellBox[S[P]]) : Unit = {
    for { 
      lc <- box.labelComplex
    } {

      val frameNesting = lc.tail.head

      // val panel = ActivePanel(frameNesting)
      // panel.refresh

      // def defaultLeafDecs : Tree[TriangleDec, P] =
      //   frameNesting match {
      //     case Box(bv, cn) => cn map (_ => Nonexistant)
      //     case _ => throw new IllegalArgumentException("Malformed complex")
      //   }

      // panel.onBoxClicked = { (b: SimpleActiveCellBox[editor.OptA[P], P]) => {

      //   val curMk : SimpleCellMarker[P] =
      //     box.optLabel match {
      //       case None => SimpleCellMarker("", DefaultColorSpec, Nonexistant, Some(defaultLeafDecs))
      //       case Some(SimpleCellMarker(l, s, r, None)) => SimpleCellMarker(l, s, r, Some(defaultLeafDecs))
      //       case Some(mk @ SimpleCellMarker(l, s, r, Some(_))) => mk
      //     }

      //   if (b.isExternal) {

      //     for {
      //       lds <- fromOpt(curMk.leafEdgeDecorations)
      //       zp <- lds.seekTo(b.address.head)
      //       d <- zp._1.rootValue
      //     } {

      //       val newLds = Zipper.close(p)(zp._2, zp._1.withRootValue(d.next))
      //       box.optLabel = Some(curMk.copy(leafEdgeDecorations = Some(newLds)))

      //       box.panel.refresh
      //       tab.editor.refreshGallery
      //       refreshFaceGallery

      //     }

      //   } else {

      //     box.optLabel = Some(curMk.copy(rootEdgeDecoration = curMk.rootEdgeDecoration.next))

      //     box.panel.refresh
      //     tab.editor.refreshGallery
      //     refreshFaceGallery

      //   }

      // }}

      // jQuery("#edge-prop-tab").empty().append(panel.element.uiElement)

      showFace

    }
  }



  // def displayCell: Unit = 
  //   for {
  //     tab <- activeTab
  //     bs <- tab.activeBox
  //   } {

  //     import tab._
  //     implicit val bsDim = bs.n

  //     val lblStr = (bs.value.optLabel map (_.label)) getOrElse ""
  //     jQuery("#label-input").value(lblStr)

  //     @natElim
  //     def doRefresh[N <: Nat](n: N)(box: tab.editor.CardinalCellBox[N]) : Unit = {
  //       case (Z, box) => {
  //         jQuery("#edge-prop-tab").empty().text("Cell is an object")
  //         refreshFaceGallery
  //       }
  //       case (S(p: P), box) => {
  //         for {
  //           lc <- box.labelComplex
  //         } {

  //           val frameNesting = lc.tail.head

  //           val panel = ActivePanel(frameNesting)
  //           panel.refresh

  //           def defaultLeafDecs : Tree[TriangleDec, P] = 
  //             frameNesting match {
  //               case Box(bv, cn) => cn map (_ => Nonexistant)
  //               case _ => throw new IllegalArgumentException("Malformed complex")
  //             }

  //           panel.onBoxClicked = { (b: SimpleActiveCellBox[editor.OptA[P], P]) => {

  //             val curMk : SimpleCellMarker[P] = 
  //               box.optLabel match {
  //                 case None => SimpleCellMarker("", DefaultColorSpec, Nonexistant, Some(defaultLeafDecs))
  //                 case Some(SimpleCellMarker(l, s, r, None)) => SimpleCellMarker(l, s, r, Some(defaultLeafDecs))
  //                 case Some(mk @ SimpleCellMarker(l, s, r, Some(_))) => mk
  //               }

  //             if (b.isExternal) {

  //               for {
  //                 lds <- fromOpt(curMk.leafEdgeDecorations)
  //                 zp <- lds.seekTo(b.address.head)
  //                 d <- zp._1.rootValue
  //               } {

  //                 val newLds = Zipper.close(p)(zp._2, zp._1.withRootValue(d.next))
  //                 box.optLabel = Some(curMk.copy(leafEdgeDecorations = Some(newLds)))

  //                 box.panel.refresh
  //                 tab.editor.refreshGallery
  //                 refreshFaceGallery

  //               }

  //             } else {

  //               box.optLabel = Some(curMk.copy(rootEdgeDecoration = curMk.rootEdgeDecoration.next))

  //               box.panel.refresh
  //               tab.editor.refreshGallery
  //               refreshFaceGallery

  //             }

  //           }}

  //           jQuery("#edge-prop-tab").empty().append(panel.element.uiElement)
  //           refreshFaceGallery

  //         }
  //       }
  //     }

  //     doRefresh(bs.n)(bs.value)

  //   }


  def showFace: Unit = 
    for {
      instance <- activeEditor
      boxsig <- instance.rootBox
      lc <- boxsig.value.labelComplex
    } { Sketchpad.viewer.complex = Some(lc) }


}
