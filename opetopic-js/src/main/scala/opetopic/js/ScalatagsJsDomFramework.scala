/**
  * ScalatagsJsDomFramework.scala - JS Dom backend for scalatags framework
  * 
  * @author Eric Finster
  * @version 0.1 
  */

import opetopic.ui._

object ScalatagsJsDomFramework extends ScalatagsFramework(scalatags.JsDom) {

  implicit val defaultPanelConfig =
    PanelConfig(
      internalPadding = 200,
      externalPadding = 400,
      leafWidth = 100,
      strokeWidth = 60,
      cornerRadius = 100
    )

  implicit val defaultGalleryConfig =
    GalleryConfig(
      panelConfig = defaultPanelConfig,
      width = 800,
      height = 600,
      spacing = 800,
      minViewX = None,
      minViewY = None,
      spacerBounds = Bounds(100, 100, 100, 100)
    )

}
