package view.scene

import scalafx.scene.control.{Button, Label, Separator, ToolBar}
import scalafx.scene.layout.{BorderPane, Pane}
import scalafx.scene.paint.Color
import scalafx.scene.text.Text

/**
 * BorderPane with box for managing simulation
 * and a short legend to understand color entities.
 */
case class SimulationPane() extends BorderPane {
  /* Custom canvas */
  private val canvas = new MyrmidonsCanvas

  /* ToolBar for manage ant simulation */
  private val toolBox: ToolBar = new ToolBar {
    val generation = new Text("0")
    val population = new Text("0")
    val startButton = new Button("Start")
    val stopButton = new Button("Stop")
    content = List(
      startButton,
      stopButton,
      new Separator,
      new Label("generation:"),
      generation,
      new Label("population:"),
      population)
  }

  /* ToolBar for understand entities color */
  private val legendBox: ToolBar = new ToolBar {
    val ant = new Text("Ant")
    val food = new Text("Food")
    val obstacle = new Text("Obstacle")
    val anthill = new Text("Anthill")
    val insect = new Text("Insect")
    val pheromone = new Text("Pheromone")
    food.fill = Color.Red
    ant.fill = Color.Black
    obstacle.fill = Color.Brown
    insect.fill = Color.Grey
    anthill.fill = Color.Green
    pheromone.fill = Color.Violet

    content = List(
      ant, new Separator,
      food, new Separator,
      obstacle, new Separator,
      anthill, new Separator,
      insect, new Separator,
      pheromone)
  }

  top = toolBox
  center = new Pane {
    children = canvas
  }
  bottom = legendBox

  /* Initialize Canvas with predefined entities */
  canvas.initializeCanvas()

}
