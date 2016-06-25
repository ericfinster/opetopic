/**
  * Tutorial.scala - Main Entry for Opetopic Tutorial
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.docs

import scala.scalajs.{js => sjs}
import sjs.Dynamic.{literal => lit}
import scala.scalajs.js.JSApp
import org.scalajs.dom._
import org.scalajs.jquery._
import scalatags.JsDom.all._

import opetopic._
import opetopic.ui._
import opetopic.js._
import JQuerySemanticUI._
import JsDomFramework._

object Docs extends JSApp {

  import Examples._

  type DocsViewer = JsStableViewer[String]
  type DocsCell = DocsViewer#CellType

  object TutorialRenderable extends Renderable[String] {
    def render(f: UIFramework)(s: String): f.CellRendering = 
      f.CellRendering(f.text(s), TutorialColorSpec)
  }

  object TutorialColorSpec extends ColorSpec(
    fill = "#f5f5f5",
    fillHovered = "#f19091",
    fillSelected = "#DCDDDE",
    stroke = "#000000",
    strokeHovered = "#000000",
    strokeSelected = "#000000"
  )

  def newViewer: DocsViewer = 
    new JsStableViewer[String]()(TutorialRenderable)

  def main: Unit = {

    println("Started Opetopic Interactive Documentation ...")

    jQuery("#nav-accordion").accordion()

    for {
      pageName <- jQuery("meta[name=page]").attr("content").toOption
    } {
      pageName match {
        case "basicediting" => runBasicEditingPage
        case "diagrams/complexes" => doComplexes
        case "diagrams/opetopes" => doOpetopes
        case "diagrams/geometry" => doGeometry
        case "categories/extrusions" => doExtrusions
        case _ => ()
      }
    }

  }

  def doOpetopes: Unit = {

    val viewer = newViewer 
    jQuery("#gallery-pane").append(viewer.uiElement)
    viewer.viewerHeight = 320
    viewer.initialize

    viewer.complex = Some(threecell)

    val faceViewer = newViewer
    jQuery("#face-pane").append(faceViewer.uiElement)
    faceViewer.initialize

    viewer.onSelectAsRoot = (c: DocsCell) => {
      for { lc <- c.face } { faceViewer.complex = Some(lc) }
    }

  }
    
  def runBasicEditingPage: Unit = {

    // val editor = new DocsEditor

    // jQuery("#editor-div").append(editor.uiElement)
    // editor.initialize

  }

  def doComplexes: Unit = {

    jQuery(".ui.checkbox").checkbox(lit(

      onChecked = () => {
        println("checked")
        jQuery("#atoms").fadeTo("slow", 0.0)
        jQuery("#bond").fadeTo("slow", 1.0)
      },

      onUnchecked = () => {
        println("unchecked")
        jQuery("#atoms").fadeTo("slow", 1.0)
        jQuery("#bond").fadeTo("slow", 0.0)
      }

    ))

    jQuery("#cbtn").on("click", () => {
      jQuery("#cbtn").addClass("active")
      jQuery("#fbtn").removeClass("active")
      jQuery("#sbtn").removeClass("active")
      jQuery("#csvg").fadeTo("slow", 1.0)
      jQuery("#fsvg").fadeTo("slow", 0.0)
      jQuery("#ssvg").fadeTo("slow", 0.0)
    })

    jQuery("#fbtn").on("click", () => {
      jQuery("#cbtn").removeClass("active")
      jQuery("#fbtn").addClass("active")
      jQuery("#sbtn").removeClass("active")
      jQuery("#csvg").fadeTo("slow", 0.0)
      jQuery("#fsvg").fadeTo("slow", 1.0)
      jQuery("#ssvg").fadeTo("slow", 0.0)
    })

    jQuery("#sbtn").on("click", () => {
      jQuery("#cbtn").removeClass("active")
      jQuery("#fbtn").removeClass("active")
      jQuery("#sbtn").addClass("active")
      jQuery("#csvg").fadeTo("slow", 0.0)
      jQuery("#fsvg").fadeTo("slow", 0.0)
      jQuery("#ssvg").fadeTo("slow", 1.0)
    })

  }


  def doExtrusions: Unit = {

    val dur: Int = 500

    setupLeftExtrusion
    setupRightExtrusion

    def setupLeftExtrusion: Unit = {

      //============================================================================================
      // LEFT EXTENSION
      //

      val path3412start = "m 8304.014,-11547.336 0,2335.1982 c 0,26.1799 5.2055,52.3496 15.2241,76.5367 10.0186,24.1871 24.8425,46.3726 43.3545,64.8847 18.5121,18.512 40.6976,33.3359 64.8847,43.3545 24.1871,10.0186 50.3568,15.2241 76.5367,15.2241 l 400,0"
      val path3408start = "m 11058.014,-11547.336 0,2335.1982 c 0,26.1799 -5.205,52.3496 -15.224,76.5367 -10.019,24.1871 -24.843,46.3726 -43.355,64.8847 -18.512,18.512 -40.697,33.3359 -64.884,43.3545 -24.187,10.0186 -50.357,15.2241 -76.537,15.2241 l -400,0"
      val path3400start = "m 7704.014,-6076.1378 0,2204.3963"

      val path3412end = "m 8304.014,-12483.138 v 3271.0002 a 200,200 0 0 0 200,200 h 400"
      val path3408end = "m 11058.014,-12483.138 v 3271.0002 a 200,200 0 0 1 -200,200 h -400"
      val path3400end = "m 7704.014,-6076.1378 v 4076"

      val path3468start = "m 14658.014,-10786.414 0,1844.2762 c 0,26.1799 5.205,52.3496 15.224,76.5367 10.019,24.1871 24.843,46.3726 43.355,64.8847 18.512,18.512 40.697,33.3359 64.884,43.3545 24.187,10.0186 50.357,15.2241 76.537,15.2241 l 400,0"
      val path3464start = "m 15924.014,-10786.414 0,1174.2762"
      val path3460start = "m 17190.014,-10786.414 0,1844.2762 c 0,26.1799 -5.205,52.3496 -15.224,76.5367 -10.019,24.1871 -24.843,46.3726 -43.355,64.8847 -18.512,18.512 -40.697,33.3359 -64.884,43.3545 -24.187,10.0186 -50.357,15.2241 -76.537,15.2241 l -400,0"
      val path3456start = "m 15924.014,-7872.1378 0,1001.0579"

      val path3468end = "m 14658.014,-11512.138 v 2570.0002 a 200,200 0 0 0 200,200 h 400"
      val path3464end = "m 15924.014,-11512.138 v 1900.0002"
      val path3460end = "m 17190.014,-11512.138 v 2570.0002 a 200,200 0 0 1 -200,200 h -400"
      val path3456end = "m 15924.014,-7872.1378 v 600"
      val path3452end = "m 15924.014,-5672.1378 v 2700"

      val lextSnap = Snap("#lext-svg")
      Snap.load("/assets/svgs/leftextension.svg", (f: Fragment) => {
        lextSnap.append(f)
        lextSnap.selectAll(".step-one").attr(lit(opacity = 0.0))
        lextSnap.selectAll(".step-two").attr(lit(opacity = 0.0))
        lextSnap.select("#f-group").attr(lit(transform = "t0,1000"))
        lextSnap.select("#path3412").attr(lit(d = path3412start))
        lextSnap.select("#path3408").attr(lit(d = path3408start))
        lextSnap.select("#path3400").attr(lit(d = path3400start))
        lextSnap.select("#path3468").attr(lit(d = path3468start))
        lextSnap.select("#path3464").attr(lit(d = path3464start))
        lextSnap.select("#path3460").attr(lit(d = path3460start))
        lextSnap.select("#path3456").attr(lit(d = path3456start))
      })

      jQuery("#start-left").on("click", () => {
        lextSnap.select("#f-group").animate(lit(transform = "t0,1000"), dur)
        lextSnap.selectAll(".step-one").animate(lit(opacity = 0.0), dur)
        lextSnap.selectAll(".step-two").animate(lit(opacity = 0.0), dur)
        lextSnap.select("#path3412").animate(lit(d = path3412start), dur)
        lextSnap.select("#path3408").animate(lit(d = path3408start), dur)
        lextSnap.select("#path3400").animate(lit(d = path3400start), dur)
        lextSnap.select("#path3468").animate(lit(d = path3468start), dur)
        lextSnap.select("#path3464").animate(lit(d = path3464start), dur)
        lextSnap.select("#path3460").animate(lit(d = path3460start), dur)
        lextSnap.select("#path3456").animate(lit(d = path3456start), dur)
        jQuery("#start-left").addClass("active")
        jQuery("#extrude-left").removeClass("active")
        jQuery("#enclose-left").removeClass("active")
      })

      jQuery("#extrude-left").on("click", () => {
        lextSnap.select("#f-group").animate(lit(transform = "t0,0"), dur)
        lextSnap.selectAll(".step-one").animate(lit(opacity = 1.0), dur)
        lextSnap.selectAll(".step-two").animate(lit(opacity = 0.0), dur)
        lextSnap.select("#path3412").animate(lit(d = path3412end), dur)
        lextSnap.select("#path3408").animate(lit(d = path3408end), dur)
        lextSnap.select("#path3400").animate(lit(d = path3400end), dur)
        lextSnap.select("#path3468").animate(lit(d = path3468end), dur)
        lextSnap.select("#path3464").animate(lit(d = path3464end), dur)
        lextSnap.select("#path3460").animate(lit(d = path3460end), dur)
        lextSnap.select("#path3456").animate(lit(d = path3456end), dur)
        jQuery("#start-left").removeClass("active")
        jQuery("#extrude-left").addClass("active")
        jQuery("#enclose-left").removeClass("active")
      })

      jQuery("#enclose-left").on("click", () => {
        lextSnap.select("#f-group").animate(lit(transform = "t0,0"), dur)
        lextSnap.selectAll(".step-one").animate(lit(opacity = 1.0), dur)
        lextSnap.selectAll(".step-two").animate(lit(opacity = 1.0), dur)
        lextSnap.select("#path3412").animate(lit(d = path3412end), dur)
        lextSnap.select("#path3408").animate(lit(d = path3408end), dur)
        lextSnap.select("#path3400").animate(lit(d = path3400end), dur)
        lextSnap.select("#path3468").animate(lit(d = path3468end), dur)
        lextSnap.select("#path3464").animate(lit(d = path3464end), dur)
        lextSnap.select("#path3460").animate(lit(d = path3460end), dur)
        lextSnap.select("#path3456").animate(lit(d = path3456end), dur)
        jQuery("#start-left").removeClass("active")
        jQuery("#extrude-left").removeClass("active")
        jQuery("#enclose-left").addClass("active")
      })

    }

    def setupRightExtrusion: Unit = {

      //============================================================================================
      // RIGHT EXTENSION
      //

      val xboxEndBounds = Bounds(4834, -10522, 2815, 3672)
      val xboxStartBounds = Bounds(5394, -8884, 1619, 1608)

      val wboxEndBounds = Bounds(4235, -11222, 7869, 7813)
      val wboxStartBounds = Bounds(4235, -9834, 7869, 6425)

      val path3370end = "m 6234.9224,-8222.707 v 2504 a 200,200 0 0 0 200,200 h 1066"
      val path3366end = "m 8849.9224,-12422.707 v 4501 a 200,200 0 0 0 200,200 h 400"
      val path3362end = "m 11603.923,-12422.707 v 4501 a 200,200 0 0 1 -200,200 h -400.001"

      val path3370start = "m 6234.9224,-7258.5077 0,1539.8007 c 0,26.1799 5.2055,52.3496 15.2241,76.5367 10.0186,24.1871 24.8425,46.3726 43.3545,64.8847 18.5121,18.512 40.6976,33.3359 64.8847,43.3545 24.1871,10.0186 50.3568,15.2241 76.5367,15.2241 l 1066,0"
      val path3366start = "m 8849.9224,-10879.988 0,2958.281 c 0,26.1799 5.2055,52.3496 15.2241,76.5367 10.0186,24.1871 24.8425,46.3726 43.3545,64.8847 18.5121,18.512 40.6976,33.3359 64.8847,43.3545 24.1871,10.0186 50.3568,15.2241 76.5367,15.2241 l 400,0"
      val path3362start = "m 11603.923,-10879.988 0,2958.281 c 0,26.1799 -5.205,52.3496 -15.224,76.5367 -10.019,24.1871 -24.843,46.3726 -43.355,64.8847 -18.512,18.512 -40.697,33.3359 -64.884,43.3545 -24.187,10.0186 -50.357,15.2241 -76.537,15.2241 l -400.001,0"

      val path3416end = "m 14876.674,-8059.4353 v 1270 a 200,200 0 0 0 200,200 h 634"
      val path3412end = "m 16376.674,-11559.435 v 4099.9997"
      val path3408end = "m 17642.674,-11559.435 v 4769.9997 a 200,200 0 0 1 -200,200 h -400"
      val path3404end = "m 16376.674,-5719.4353 v 2700"

      val path3416start = "m 14876.674,-9023.6346 0,2234.1993 c 0,26.1799 5.205,52.3496 15.224,76.5367 10.019,24.1871 24.843,46.3726 43.355,64.8847 18.512,18.512 40.697,33.3359 64.884,43.3545 24.187,10.0186 50.357,15.2241 76.537,15.2241 l 634,0"
      val path3412start = "m 16376.674,-9052.5168 0,1593.0815"
      val path3408start = "m 17642.674,-9052.5168 0,2263.0815 c 0,26.1799 -5.205,52.3496 -15.224,76.5367 -10.019,24.1871 -24.843,46.3726 -43.355,64.8847 -18.512,18.512 -40.697,33.3359 -64.884,43.3545 -24.187,10.0186 -50.357,15.2241 -76.537,15.2241 l -400,0"
      val path3404start = "m 16376.674,-5719.4353 0,1562.2448"

      val rextSnap = Snap("#rext-svg")
      Snap.load("/assets/svgs/rightextension.svg", (f: Fragment) => {
        rextSnap.append(f)
        rextSnap.selectAll(".step-one").attr(lit(opacity = 0.0))
        rextSnap.selectAll(".step-two").attr(lit(opacity = 0.0))
        setupBox("#rect3346", xboxStartBounds)
        setupBox("#rect3338", wboxStartBounds)
        rextSnap.select("#path3370").attr(lit(d = path3370start))
        rextSnap.select("#path3366").attr(lit(d = path3366start))
        rextSnap.select("#path3362").attr(lit(d = path3362start))
        rextSnap.select("#path3350").attr(lit(transform = "translate(-700,-500)"))
        rextSnap.select("#path3416").attr(lit(d = path3416start))
        rextSnap.select("#path3412").attr(lit(d = path3412start))
        rextSnap.select("#path3408").attr(lit(d = path3408start))
        rextSnap.select("#path3404").attr(lit(d = path3404start))
      })

      def animateBox(id: String, bnds: Bounds): Unit =
        rextSnap.select(id).animate(lit(x = bnds.x, y = bnds.y, width = bnds.width, height = bnds.height), dur)

      def setupBox(id: String, bnds: Bounds): Unit =
        rextSnap.select(id).attr(lit(x = bnds.x, y = bnds.y, width = bnds.width, height = bnds.height))

      jQuery("#start-right").on("click", () => {
        rextSnap.selectAll(".step-one").animate(lit(opacity = 0.0), dur)
        rextSnap.selectAll(".step-two").animate(lit(opacity = 0.0), dur)
        animateBox("#rect3346", xboxStartBounds)
        animateBox("#rect3338", wboxStartBounds)
        rextSnap.select("#path3370").animate(lit(d = path3370start), dur)
        rextSnap.select("#path3366").animate(lit(d = path3366start), dur)
        rextSnap.select("#path3362").animate(lit(d = path3362start), dur)
        rextSnap.select("#path3350").animate(lit(transform = "translate(-700,-500)"), dur)
        rextSnap.select("#path3416").animate(lit(d = path3416start), dur)
        rextSnap.select("#path3412").animate(lit(d = path3412start), dur)
        rextSnap.select("#path3408").animate(lit(d = path3408start), dur)
        rextSnap.select("#path3404").animate(lit(d = path3404start), dur)
        jQuery("#start-right").addClass("active")
        jQuery("#extrude-right").removeClass("active")
        jQuery("#enclose-right").removeClass("active")
      })

      jQuery("#extrude-right").on("click", () => {
        rextSnap.selectAll(".step-one").animate(lit(opacity = 1.0), dur)
        rextSnap.selectAll(".step-two").animate(lit(opacity = 0.0), dur)
        animateBox("#rect3346", xboxEndBounds)
        animateBox("#rect3338", wboxEndBounds)
        rextSnap.select("#path3370").animate(lit(d = path3370end), dur)
        rextSnap.select("#path3366").animate(lit(d = path3366end), dur)
        rextSnap.select("#path3362").animate(lit(d = path3362end), dur)
        rextSnap.select("#path3350").animate(lit(transform = "translate(0,0)"), dur)
        rextSnap.select("#path3416").animate(lit(d = path3416end), dur)
        rextSnap.select("#path3412").animate(lit(d = path3412end), dur)
        rextSnap.select("#path3408").animate(lit(d = path3408end), dur)
        rextSnap.select("#path3404").animate(lit(d = path3404end), dur)
        jQuery("#start-right").removeClass("active")
        jQuery("#extrude-right").addClass("active")
        jQuery("#enclose-right").removeClass("active")
      })

      jQuery("#enclose-right").on("click", () => {
        rextSnap.selectAll(".step-one").animate(lit(opacity = 1.0), dur)
        rextSnap.selectAll(".step-two").animate(lit(opacity = 1.0), dur)
        animateBox("#rect3346", xboxEndBounds)
        animateBox("#rect3338", wboxEndBounds)
        rextSnap.select("#path3370").animate(lit(d = path3370end), dur)
        rextSnap.select("#path3366").animate(lit(d = path3366end), dur)
        rextSnap.select("#path3362").animate(lit(d = path3362end), dur)
        rextSnap.select("#path3350").animate(lit(transform = "translate(0,0)"), dur)
        rextSnap.select("#path3416").animate(lit(d = path3416end), dur)
        rextSnap.select("#path3412").animate(lit(d = path3412end), dur)
        rextSnap.select("#path3408").animate(lit(d = path3408end), dur)
        rextSnap.select("#path3404").animate(lit(d = path3404end), dur)
        jQuery("#start-right").removeClass("active")
        jQuery("#extrude-right").removeClass("active")
        jQuery("#enclose-right").addClass("active")
      })

    }

  }

  def doGeometry: Unit = {

    //============================================================================================
    // HELPERS
    //

    val hoveredStroke = "#ff2a2a"
    val hoveredFill = "#ff2a2a"

    val unhoveredStroke = "#000000"
    val unhoveredFill = "#000000"

    val hoveredAuxFill = TutorialColorSpec.fillHovered
    val unhoveredAuxFill = "#000000"

    def lblToClass(str: String) : String =
      str match {
        case "α" => "alpha" 
        case "β" => "beta"
        case "γ" => "gamma"
        case "δ" => "delta"
        case "ε" => "epsilon"
        case "ζ" => "zeta"
        case "Φ" => "phi"
        case _ => str
      }

    def installHandlers(viewer: DocsViewer, el: SnapElement) : Unit = {

      viewer.onHover = (c: DocsCell) => {
        el.selectAll(".stroke-" + lblToClass(c.label)).attr(lit(stroke = hoveredStroke))
        el.selectAll(".fill-" + lblToClass(c.label)).attr(lit(fill = hoveredFill))
      }

      viewer.onUnhover = (c: DocsCell) => {
        el.selectAll(".stroke-" + lblToClass(c.label)).attr(lit(stroke = unhoveredStroke))
        el.selectAll(".fill-" + lblToClass(c.label)).attr(lit(fill = unhoveredFill))
      }

    }

    //============================================================================================
    // THE OBJECT
    //

    val objectViewer = newViewer
    jQuery("#object-pane").append(objectViewer.uiElement)
    objectViewer.initialize
    objectViewer.complex = Some(obj)

    val objectEl = Snap("#object-svg")
    Snap.load("/assets/svgs/object.svg", (f: Fragment) => {
      objectEl.append(f)
    })

    installHandlers(objectViewer, objectEl)

    //============================================================================================
    // THE ARROW
    //

    val arrowViewer = newViewer
    jQuery("#arrow-pane").append(arrowViewer.uiElement)
    arrowViewer.initialize
    arrowViewer.complex = Some(arrow)

    val arrowEl = Snap("#arrow-svg")
    Snap.load("/assets/svgs/arrow.svg", (f: Fragment) => {
      arrowEl.append(f)
    })

    installHandlers(arrowViewer, arrowEl)

    //============================================================================================
    // THE DROP
    //

    val dropViewer = newViewer
    jQuery("#drop-pane").append(dropViewer.uiElement)
    dropViewer.initialize
    dropViewer.complex = Some(drop)

    val dropEl = Snap("#drop-svg")
    Snap.load("/assets/svgs/drop.svg", (f: Fragment) => {
      dropEl.append(f)
    })

    installHandlers(dropViewer, dropEl)

    //============================================================================================
    // THE TWOGLOB
    //

    val twoglobViewer = newViewer
    jQuery("#twoglob-pane").append(twoglobViewer.uiElement)
    twoglobViewer.initialize
    twoglobViewer.complex = Some(twoglob)

    val twoglobEl = Snap("#twoglob-svg")
    Snap.load("/assets/svgs/twoglob.svg", (f: Fragment) => {
      twoglobEl.append(f)
    })

    installHandlers(twoglobViewer, twoglobEl)

    //============================================================================================
    // THE SIMPLEX
    //

    val simplexViewer = newViewer
    jQuery("#simplex-pane").append(simplexViewer.uiElement)
    simplexViewer.initialize
    simplexViewer.complex = Some(simplex)

    val simplexEl = Snap("#simplex-svg")
    Snap.load("/assets/svgs/simplex.svg", (f: Fragment) => {
      simplexEl.append(f)
    })

    installHandlers(simplexViewer, simplexEl)

    //============================================================================================
    // THE QUAD
    //

    val quadViewer = newViewer
    jQuery("#quad-pane").append(quadViewer.uiElement)
    quadViewer.initialize
    quadViewer.complex = Some(quad)

    val quadEl = Snap("#quad-svg")
    Snap.load("/assets/svgs/quad.svg", (f: Fragment) => {
      quadEl.append(f)
    })

    installHandlers(quadViewer, quadEl)

    //============================================================================================
    // THE THREECELL
    //

    val threecellViewer = newViewer
    jQuery("#threecell-pane").append(threecellViewer.uiElement)
    threecellViewer.viewerHeight = 320
    threecellViewer.initialize
    threecellViewer.complex = Some(threecell)

    val threecellEl = Snap("#threecell-svg")
    Snap.load("/assets/svgs/threecell.svg", (f: Fragment) => {
      threecellEl.append(f)
    })

    installHandlers(threecellViewer, threecellEl)

  }

}

