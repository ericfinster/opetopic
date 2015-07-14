/**
  * SvgFramework.scala - Framework with scalatags backend
  * 
  * @author Eric Finster
  * @version 0.1 
  */

package opetopic.ui

object FXSVGBundle
    extends scalatags.Text.Cap
    with scalatags.text.SvgTags
    with scalatags.DataConverters
    with scalatags.Text.Aggregate{ object svgattr extends scalatags.Text.Cap with scalatags.Text.SvgAttrs }

import FXSVGBundle._

