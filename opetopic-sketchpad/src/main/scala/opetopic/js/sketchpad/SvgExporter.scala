// /**
//   * SvgExporter.scala - Simple Class for Exporting SVG's
//   * 
//   * @author Eric Finster
//   * @version 0.1 
//   */

// package opetopic.js.sketchpad

// import opetopic._
// import opetopic.ui._
// import opetopic.tt._
// import syntax.complex._
// import markers._

// import SimpleMarker._
// import ScalatagsTextFramework._

// class SvgExporter[N <: Nat](cmplx: Complex[OptMarker, N]) {

//   implicit val staticPanelConfig =
//     PanelConfig(
//       internalPadding = 400,
//       externalPadding = 600,
//       decorationPadding = 400,
//       leafWidth = 200,
//       strokeWidth = 100,
//       cornerRadius = 200
//     )

//   implicit val staticGalleryConfig =
//     GalleryConfig(
//       panelConfig = staticPanelConfig,
//       width = 1000,
//       height = 300,
//       spacing = 2000,
//       minViewX = Some(80000),
//       minViewY = Some(15000),
//       spacerBounds = Bounds(0, 0, 600, 600)
//     )

//   implicit val spacerBounds = Bounds(0, 0, 600, 600)

//   implicit val vf : VisualizableFamily[SimpleMarker] = 
//     SimpleMarker.frameworkFamily(ScalatagsTextFramework)

//   val staticGallery = SimpleStaticGallery(cmplx)

//   def svgString = staticGallery.element.toString

// }
