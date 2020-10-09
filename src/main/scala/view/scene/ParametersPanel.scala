package view.scene

import utility.Parameters.GUIConstant.SIMULATION_SIZE
import view.Myrmidons.frame.dispose

import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, Dimension, GridPanel, Label, MainFrame, TextField}

private[view] case class ParametersPanel() extends GridPanel(6, 2) {
  private val antSize = new Label("Number of Ant (F+G)")
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

  listenTo(setButton, setDefaultButton)
  reactions += {
    case ButtonClicked(component) if component == setButton =>
      showSimulationWithParameters(antSizeInput,
        anthillFoodInput, foodSizeInput,
        obstacleSizeInput, enemiesSizeInput)

    case ButtonClicked(component) if component == setDefaultButton =>
      showSimulationWithParameters(120, 5000,
        6, 5, 50)
      //TODO Decidere default number
  }

  private def showSimulationWithParameters(antSizeInput: Int,
                                           anthillFoodInput: Int, foodSizeInput: Int,
                                           obstacleSizeInput: Int, enemiesSizeInput: Int): Unit = {
    dispose()

    val frame: MainFrame = new MainFrame {

      title = "Myrmidons - Ant Simulator"

      val myrmidonsPanel: MyrmidonsPanel = MyrmidonsPanel()
      val controlPane: ControlPane = ControlPane(myrmidonsPanel)
      controlPane.setParameters(
        antSizeInput,
        anthillFoodInput, foodSizeInput,
        obstacleSizeInput, enemiesSizeInput)

      val labelPane: LabelPane = LabelPane()

      contents = new BorderPanel {
        layout += controlPane -> BorderPanel.Position.North
        layout += myrmidonsPanel -> BorderPanel.Position.Center
        layout += labelPane -> BorderPanel.Position.South
      }
      size = new Dimension(SIMULATION_SIZE._1, SIMULATION_SIZE._2)
      resizable = false
    }
    frame.visible = true
  }

  private implicit def numberFrom(component: TextField): Int = {
    if (component.text == "") {
      component.text = "MIN_COMPONENT"
    }
    component.text.toInt
  }
}
