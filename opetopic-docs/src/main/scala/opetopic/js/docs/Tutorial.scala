/**
  * Tutorial.scala - Main Entry for Opetopic Tutorial
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.js.docs

import scala.scalajs.js
import org.scalajs.jquery._
import js.Dynamic.{literal => lit}

import opetopic._
import opetopic.ui._
import opetopic.js._
import JsDomFramework._

object Tutorial {

  import Examples._

  type DocsViewer = JsStableViewer[String]
  type DocsCell = DocsViewer#CellType

  object TutorialRenderable extends Renderable[String, JsDomFramework.type] {
    def render(f: JsDomFramework.type)(s: String): f.CellRendering = 
      f.CellRendering(f.text(s), TutorialColorSpec)
  }

  object TutorialColorSpec extends ColorSpec(
    fill = "#f5f5f5",
    fillHovered = "#f19091",
    fillSelected = "#f39091",
    stroke = "#000000",
    strokeHovered = "#000000",
    strokeSelected = "#000000",
    edgeHovered = "red"
  )

  def newViewer: DocsViewer = 
    new JsStableViewer[String]()(TutorialRenderable)

  val bondViewer = newViewer
  jQuery("#bond-diagram").append(bondViewer.uiElement)
  bondViewer.viewerHeight = 320
  bondViewer.initialize
  bondViewer.firstPanel = Some(3)
  bondViewer.lastPanel = Some(4)

  val opetopeViewer = newViewer
  jQuery("#opetope-diagram").append(opetopeViewer.uiElement)
  opetopeViewer.viewerHeight = 320
  opetopeViewer.initialize

  val faceViewer = newViewer
  jQuery("#face-diagram").append(faceViewer.uiElement)
  faceViewer.viewerHeight = 180
  faceViewer.initialize

  opetopeViewer.onSelectAsRoot = (c: DocsCell) => {
    for { lc <- c.face } { faceViewer.complex = Some(lc) }
  }

  //============================================================================================
  // VIEWER HANDLERS
  //

  def installHandlers(viewer: DocsViewer, el: SnapElement) : Unit = {

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

  val threecellEl = Snap("#threecell-svg")
  Snap.load("/assets/svgs/threecell.svg", (f: Fragment) => {
    threecellEl.append(f)
  })

  installHandlers(threecellViewer, threecellEl)

  //============================================================================================
  // INITIALIZATION
  //

  def start = {

    // Start the reveal.js presentation viewer
    Reveal.initialize()

    Reveal.addEventListener(
      "slidechanged",
      slideChangedHandler,
      false
    )

    Reveal.addEventListener(
      "fragmentshown",
      fragmentShownHandler,
      false
    )

    Reveal.addEventListener(
      "fragmenthidden",
      fragmentHiddenHandler,
      false
    )
    
  }

  val slideChangedHandler : RevealEvent => js.Any =
    (ev: RevealEvent) => {
      jQuery(ev.currentSlide).attr("id").toOption match {
        case Some("bond-diagram-slide") => { bondViewer.complex = Some(big) }
        case Some("opetope-diagram-slide") => { opetopeViewer.complex = Some(big) }
        case Some("object-slide") => { objectViewer.complex = Some(obj) }
        case Some("arrow-slide") => { arrowViewer.complex = Some(arrow) }
        case Some("drop-slide") => { dropViewer.complex = Some(drop) }
        case Some("twoglob-slide") => { twoglobViewer.complex = Some(twoglob) }
        case Some("simplex-slide") => { simplexViewer.complex = Some(simplex) }
        case Some("quad-slide") => { quadViewer.complex = Some(quad) }
        case Some("threecell-slide") => { threecellViewer.complex = Some(threecell) }
        case _ => ()
      }
    }

  val fragmentShownHandler : RevealEvent => js.Any =
    (ev: RevealEvent) => {
      jQuery(ev.fragment).attr("id").toOption match {
        case Some("bond-fragment") => {
          jQuery("#atoms").fadeTo("slow", 0.0)
          jQuery("#bond").fadeTo("slow", 1.0)
        }
        case Some("lower-bond-fragment") => {
          jQuery("#csvg").fadeTo("slow", 0.0)
          jQuery("#fsvg").fadeTo("slow", 1.0)
          jQuery("#ssvg").fadeTo("slow", 0.0)
        }
        case Some("upper-bond-fragment") => {
          jQuery("#csvg").fadeTo("slow", 0.0)
          jQuery("#fsvg").fadeTo("slow", 0.0)
          jQuery("#ssvg").fadeTo("slow", 1.0)
        }
        case Some("tgt-1-fragment") => {
          jQuery("#tgt-prop-1").fadeTo("slow", 0.0)
          jQuery("#tgt-prop-2").fadeTo("slow", 1.0)
        }
        case Some("tgt-2-fragment") => {
          jQuery("#tgt-prop-2").fadeTo("slow", 0.0)
          jQuery("#tgt-prop-3").fadeTo("slow", 1.0)
        }
        case Some("tgt-3-fragment") => {
          jQuery("#tgt-prop-3").fadeTo("slow", 0.0)
          jQuery("#tgt-prop-4").fadeTo("slow", 1.0)
        }
        case Some("tgt-4-fragment") => {
          jQuery("#tgt-prop-4").fadeTo("slow", 0.0)
          jQuery("#tgt-prop-5").fadeTo("slow", 1.0)
        }
        case Some("tgt-5-fragment") => {
          jQuery("#tgt-prop-5").fadeTo("slow", 0.0)
          jQuery("#tgt-prop-6").fadeTo("slow", 1.0)
        }
        case Some("src-1-fragment") => {
          jQuery("#src-prop-1").fadeTo("slow", 0.0)
          jQuery("#src-prop-2").fadeTo("slow", 1.0)
        }
        case Some("src-2-fragment") => {
          jQuery("#src-prop-2").fadeTo("slow", 0.0)
          jQuery("#src-prop-3").fadeTo("slow", 1.0)
        }
        case Some("src-3-fragment") => {
          jQuery("#src-prop-3").fadeTo("slow", 0.0)
          jQuery("#src-prop-4").fadeTo("slow", 1.0)
        }
        case Some("src-4-fragment") => {
          jQuery("#src-prop-4").fadeTo("slow", 0.0)
          jQuery("#src-prop-5").fadeTo("slow", 1.0)
        }
        case Some("src-5-fragment") => {
          jQuery("#src-prop-5").fadeTo("slow", 0.0)
          jQuery("#src-prop-6").fadeTo("slow", 1.0)
        }
        case Some("comps-fragment") => {
          jQuery("#comps-1").fadeTo("slow", 0.0)
          jQuery("#comps-2").fadeTo("slow", 1.0)
        }
        case Some("eqvs-1-fragment") => {
          jQuery("#eqvs-1").fadeTo("slow", 0.0)
          jQuery("#eqvs-2").fadeTo("slow", 1.0)
        }
        case Some("eqvs-2-fragment") => {
          jQuery("#eqvs-2").fadeTo("slow", 0.0)
          jQuery("#eqvs-3").fadeTo("slow", 1.0)
        }
        case Some("eqvs-3-fragment") => {
          jQuery("#eqvs-3").fadeTo("slow", 0.0)
          jQuery("#eqvs-4").fadeTo("slow", 1.0)
        }
        case _ => println("unknown fragment")
      }

    }

  val fragmentHiddenHandler : RevealEvent => js.Any =
    (ev: RevealEvent) => {
      jQuery(ev.fragment).attr("id").toOption match {
        case Some("bond-fragment") => {
          jQuery("#atoms").fadeTo("slow", 1.0)
          jQuery("#bond").fadeTo("slow", 0.0)
        }
        case Some("lower-bond-fragment") => {
          jQuery("#csvg").fadeTo("slow", 1.0)
          jQuery("#fsvg").fadeTo("slow", 0.0)
          jQuery("#ssvg").fadeTo("slow", 0.0)
        }
        case Some("upper-bond-fragment") => {
          jQuery("#csvg").fadeTo("slow", 0.0)
          jQuery("#fsvg").fadeTo("slow", 1.0)
          jQuery("#ssvg").fadeTo("slow", 0.0)
        }
        case Some("tgt-1-fragment") => {
          jQuery("#tgt-prop-1").fadeTo("slow", 1.0)
          jQuery("#tgt-prop-2").fadeTo("slow", 0.0)
        }
        case Some("tgt-2-fragment") => {
          jQuery("#tgt-prop-2").fadeTo("slow", 1.0)
          jQuery("#tgt-prop-3").fadeTo("slow", 0.0)
        }
        case Some("tgt-3-fragment") => {
          jQuery("#tgt-prop-3").fadeTo("slow", 1.0)
          jQuery("#tgt-prop-4").fadeTo("slow", 0.0)
        }
        case Some("tgt-4-fragment") => {
          jQuery("#tgt-prop-4").fadeTo("slow", 1.0)
          jQuery("#tgt-prop-5").fadeTo("slow", 0.0)
        }
        case Some("tgt-5-fragment") => {
          jQuery("#tgt-prop-5").fadeTo("slow", 1.0)
          jQuery("#tgt-prop-6").fadeTo("slow", 0.0)
        }
        case Some("src-1-fragment") => {
          jQuery("#src-prop-1").fadeTo("slow", 1.0)
          jQuery("#src-prop-2").fadeTo("slow", 0.0)
        }
        case Some("src-2-fragment") => {
          jQuery("#src-prop-2").fadeTo("slow", 1.0)
          jQuery("#src-prop-3").fadeTo("slow", 0.0)
        }
        case Some("src-3-fragment") => {
          jQuery("#src-prop-3").fadeTo("slow", 1.0)
          jQuery("#src-prop-4").fadeTo("slow", 0.0)
        }
        case Some("src-4-fragment") => {
          jQuery("#src-prop-4").fadeTo("slow", 1.0)
          jQuery("#src-prop-5").fadeTo("slow", 0.0)
        }
        case Some("src-5-fragment") => {
          jQuery("#src-prop-5").fadeTo("slow", 1.0)
          jQuery("#src-prop-6").fadeTo("slow", 0.0)
        }
        case Some("comps-fragment") => {
          jQuery("#comps-1").fadeTo("slow", 1.0)
          jQuery("#comps-2").fadeTo("slow", 0.0)
        }
        case Some("eqvs-1-fragment") => {
          jQuery("#eqvs-1").fadeTo("slow", 1.0)
          jQuery("#eqvs-2").fadeTo("slow", 0.0)
        }
        case Some("eqvs-2-fragment") => {
          jQuery("#eqvs-2").fadeTo("slow", 1.0)
          jQuery("#eqvs-3").fadeTo("slow", 0.0)
        }
        case Some("eqvs-3-fragment") => {
          jQuery("#eqvs-3").fadeTo("slow", 1.0)
          jQuery("#eqvs-4").fadeTo("slow", 0.0)
        }
        case _ => println("unknown fragment")
      }
    }

}
