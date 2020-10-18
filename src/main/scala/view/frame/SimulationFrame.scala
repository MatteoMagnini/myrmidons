package view.frame

import view.scene.{ControlPanel, LabelPanel, MyrmidonsPanel}

import scala.swing.{BorderPanel, Dimension, MainFrame}

trait SimulationFrame extends MainFrame {

  def antSizeInput: Int

  def anthillFoodInput: Int

  def foodSizeInput: Int

  def obstacleSizeInput: Int

  def enemiesSizeInput: Int
}

object SimulationFrame {

  def apply(antSizeInput: Int, anthillFoodInput: Int,
            foodSizeInput: Int, obstacleSizeInput: Int,
            enemiesSizeInput: Int): SimulationFrame =
    new SimulationFrameImpl(antSizeInput, anthillFoodInput,
      foodSizeInput, obstacleSizeInput, enemiesSizeInput)

  /**
   * Frame that contains labels, control and panel where entities will be draw.
   *
   * @param antSizeInput      Parameters for ant size.
   * @param anthillFoodInput  Parameters for anthill food  size.
   * @param foodSizeInput     Parameters for food size.
   * @param obstacleSizeInput Parameters for obstacle size.
   * @param enemiesSizeInput  Parameters for enemies size
   */
  private[view] class SimulationFrameImpl(override val antSizeInput: Int,
                                          override val anthillFoodInput: Int,
                                          override val foodSizeInput: Int,
                                          override val obstacleSizeInput: Int,
                                          override val enemiesSizeInput: Int)
    extends SimulationFrame {
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
    size = new Dimension(SIMULATION_SIZE.width, SIMULATION_SIZE.height)
    resizable = false
  }

}

