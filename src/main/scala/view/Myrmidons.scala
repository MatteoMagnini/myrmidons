package view

import view.scene.ParametersPanel

import scala.swing.{BorderPanel, Dimension, MainFrame}

/**
 * Simulation entry point.
 */
object Myrmidons extends App {

  val frame: MainFrame = new MainFrame {
    title = "Myrmidons - Set Parameters"
    val settingPanel: ParametersPanel = ParametersPanel()
    contents = new BorderPanel {
      layout += settingPanel -> BorderPanel.Position.Center
    }
    size = new Dimension(SETTING_SIZE, SETTING_SIZE)
    resizable = false
  }
  frame.visible = true

}
