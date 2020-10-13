package view.frame

import view.SIMULATION_SIZE
import view.scene.{ControlPanel, LabelPanel, MyrmidonsPanel}

import scala.swing.{BorderPanel, Dimension, MainFrame}

private[view] case class SimulationFrame(antSizeInput: Int, anthillFoodInput: Int,
                                         foodSizeInput: Int, obstacleSizeInput: Int,
                                         enemiesSizeInput: Int) extends MainFrame {
  title = "Myrmidons - Ant Simulator"
  val myrmidonsPanel: MyrmidonsPanel = MyrmidonsPanel()
  val controlPane: ControlPanel = ControlPanel(myrmidonsPanel)
  val labelPane: LabelPanel = LabelPanel()
  controlPane.setParameters(antSizeInput, anthillFoodInput, foodSizeInput,
    obstacleSizeInput, enemiesSizeInput)

  contents = new BorderPanel {
    layout += controlPane -> BorderPanel.Position.North
    layout += myrmidonsPanel -> BorderPanel.Position.Center
    layout += labelPane -> BorderPanel.Position.South
  }
  size = new Dimension(SIMULATION_SIZE._1, SIMULATION_SIZE._2)
  resizable = false
}
