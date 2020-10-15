package view.frame

import view.scene.ParametersPanel

import scala.swing.{BorderPanel, Dimension, MainFrame}
trait SettingFrame extends MainFrame{}
object SettingFrame{

  def apply(): SettingFrame = new SettingFrameImpl()
  /**
   *
   */
  private[view] class SettingFrameImpl () extends SettingFrame {
    title = "Myrmidons - Set Parameters"
    val settingPanel: ParametersPanel = ParametersPanel(this)
    contents = new BorderPanel {
      layout += settingPanel -> BorderPanel.Position.Center
    }
    size = new Dimension(SETTING_SIZE, SETTING_SIZE)
    resizable = false
    visible = true
  }
}
