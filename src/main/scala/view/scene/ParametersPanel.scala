package view.scene


import view.frame.{SettingFrame, SimulationFrame}

import scala.swing.event.ButtonClicked
import scala.swing.{Button, GridPanel, Label, TextField}

trait ParametersPanel extends GridPanel {
  def frame: SettingFrame
}

object ParametersPanel {

  def apply(frame: SettingFrame): ParametersPanel = new ParametersPanelImpl(frame)

  /**
   * Panel to set simulation parameters.
   *
   * @param frame Frame where append this panel and dispose when parameters are set.
   */
  private[view] class ParametersPanelImpl(override val frame: SettingFrame)
    extends GridPanel(PARAMETER_GRID._1, PARAMETER_GRID._2) with ParametersPanel {

    private val antSize = new Label("Number of Ant (F+P)")
    private val antSizeInput = new TextField()
    private val anthillFood = new Label("Number of food in anthill")
    private val anthillFoodInput = new TextField()
    private val foodSize = new Label("Number of food")
    private val foodSizeInput = new TextField()
    private val obstacleSize = new Label("Number of obstacle")
    private val obstacleSizeInput = new TextField()
    private val enemiesSize = new Label("Number of enemies")
    private val enemiesSizeInput = new TextField()
    private val setButton = new Button("Set Simulation")
    private val setDefaultButton = new Button("Set Default")

    contents ++= Seq(antSize, antSizeInput, anthillFood, anthillFoodInput, foodSize, foodSizeInput,
      obstacleSize, obstacleSizeInput, enemiesSize, enemiesSizeInput, setDefaultButton, setButton)

    /**
     * User can chose to set parameter with text field or select default parameter.
     */
    listenTo(setButton, setDefaultButton)
    reactions += {
      case ButtonClicked(`setButton`) =>
        showSimulationWithParameters(antSizeInput,
          anthillFoodInput, foodSizeInput,
          obstacleSizeInput, enemiesSizeInput)

      case ButtonClicked(`setDefaultButton`) =>
        showSimulationWithParameters(DEFAULT_ANT_SIZE, DEFAULT_ANTHILL_FOOD,
          DEFAULT_FOOD_SIZE, DEFAULT_OBSTACLE_SIZE, DEFAULT_ENEMIES_SIZE)
    }

    /**
     * Show new SimulationFrame with parameters.
     *
     * @param antSizeInput      how many ant in simulation.
     * @param anthillFoodInput  how many anthill food  in simulation.
     * @param foodSizeInput     how many food number in simulation.
     * @param obstacleSizeInput how many obstacle number in simulation.
     * @param enemiesSizeInput  how many enemies number in simulation.
     */
    private def showSimulationWithParameters(antSizeInput: Int, anthillFoodInput: Int, foodSizeInput: Int,
                                             obstacleSizeInput: Int, enemiesSizeInput: Int): Unit = {
      frame.dispose()
      val simulationFrame = SimulationFrame(antSizeInput, anthillFoodInput, foodSizeInput,
        obstacleSizeInput, enemiesSizeInput)
      simulationFrame.visible = true
    }
  }

}
