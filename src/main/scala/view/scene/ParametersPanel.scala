package view.scene

import view.Myrmidons.frame.dispose
import scala.swing.event.ButtonClicked
import scala.swing.{BorderPanel, Button, Dimension, GridPanel, Label, MainFrame, Separator, TextField}

private[view] case class ParametersPanel() extends GridPanel(6, 2) {
  private val antSize = new Label("Ant Size")
  private val antSizeInput = new TextField()
  private val anthillFood = new Label("Anthill Food")
  private val anthillFoodInput = new TextField()
  private val foodSize = new Label("Food Size")
  private val foodSizeInput = new TextField()
  private val obstacleSize = new Label("Number of obstacle")
  private val obstacleSizeInput = new TextField()
  private val enemiesSize = new Label("Number of enemies")
  private val enemiesSizeInput = new TextField()
  private val setButton = new Button("Set Simulation")

  contents ++= Seq(antSize, antSizeInput, anthillFood, anthillFoodInput, foodSize, foodSizeInput,
    obstacleSize, obstacleSizeInput, enemiesSize, enemiesSizeInput, new Separator(), setButton)

  listenTo(setButton)
  reactions += {
    case ButtonClicked(component) if component == setButton => {
      dispose()

      val frame: MainFrame = new MainFrame {

        title = "Myrmidons - Ant Simulator"
        val myrmidonsPanel: MyrmidonsPanel = MyrmidonsPanel()

        val controlPane: ControlPane = ControlPane(myrmidonsPanel, this)
        controlPane.setParameters(antSizeInput,
          anthillFoodInput, foodSizeInput,
          obstacleSizeInput, enemiesSizeInput)
        val labelPane: LabelPane = LabelPane()

        contents = new BorderPanel {
          layout += controlPane -> BorderPanel.Position.North
          layout += myrmidonsPanel -> BorderPanel.Position.Center
          layout += labelPane -> BorderPanel.Position.South
        }
        size = new Dimension(800, 900)
        resizable = false
      }
      frame.visible = true
    }
  }

  private implicit def numberFrom(component: TextField): Int = component.text.toInt
}
