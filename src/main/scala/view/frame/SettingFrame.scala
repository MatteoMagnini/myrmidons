package view.frame

import view.SETTING_SIZE
import view.scene.ParametersPanel

import scala.swing.{BorderPanel, Dimension, MainFrame}

case class SettingFrame() extends MainFrame {
  title = "Myrmidons - Set Parameters"
  val settingPanel: ParametersPanel = ParametersPanel(this)
  contents = new BorderPanel {
    layout += settingPanel -> BorderPanel.Position.Center
  }
  size = new Dimension(SETTING_SIZE, SETTING_SIZE)
  resizable = false
  visible = true
}
